{-# LANGUAGE NoOverloadedLists #-}
{- | Retries as middleware over "Effectful.Http".

A 'RetryPolicy' has three elements

* 'Classifier': is this exchange worth another go? Composable
  with @'<>'@
* 'Backoff': should there be another attempt, and how long to wait.
  A 'Monoid': @'Nothing'@ means give up and absorbs, otherwise the longer
  delay wins. @'exponential' base '<>' 'limitRetries' 5@ therefore reads as
  the conjunction of two independent limits.
* 'Jitter': how to spread the herd, since synchronised clients retrying on
  the same curve reconverge on the same instant.

@
'withRetry' 'defaultRetryPolicy' program

'withRetry'
  'defaultRetryPolicy'
    { classifier = 'idempotentOnly' '<>' 'whenStatus' ((409 ==) . 'statusCode') 'Retry'
    , backoff = 'exponential' ('milliseconds' 200) '<>' 'limitRetries' 6
    }
  program
@

A request whose body is 'Effectful.Http.OneShot' is never retried, whatever the classifier
says: the first send drains the popper, so a second would put an empty body
on the wire.
-}
module Effectful.Http.Retry (
  -- * Installing
  withRetry,
  RetryPolicy (..),
  defaultRetryPolicy,

  -- * Deciding whether
  Verdict (..),
  Classifier (..),
  classify,
  transient,
  idempotentOnly,
  never,
  whenStatus,
  retryAfter,

  -- * Deciding when
  Backoff (..),
  Attempt (..),
  constant,
  exponential,
  fibonacci,
  limitRetries,
  limitTotalDelay,
  giveUp,

  -- * Spreading the herd
  Jitter (..),
) where

import Control.Monad              (guard)
import Data.ByteString.Char8      qualified as Char8
import Data.Char                  (isSpace)
import Data.Functor               (($>))
import Effectful
import Effectful.Concurrent       (Concurrent)
import Effectful.Dispatch.Dynamic (interpose_)
import Effectful.Http             (Exchange (..), Failure (..), Http (..), Replay (Replayable), Request (..), fetch, replay)
import Effectful.Http.Duration    (Duration, delay, milliseconds, scale, seconds)
import Network.HTTP.Client        (HttpExceptionContent (..), Response, responseHeaders, responseStatus)
import Network.HTTP.Types
import System.Random.Stateful     (globalStdGen, uniformRM)

-- * Installing

data RetryPolicy = RetryPolicy
  { classifier   :: Classifier
  , backoff      :: Backoff
  , jitter       :: Jitter
  , maximumDelay :: Duration
  {- ^ Ceiling on any single sleep, applied last and therefore binding on a
  server's @Retry-After@ too. A backoff can only constrain itself.
  -}
  }

{- | Up to three retries on transient failures, exponential from 100ms, no
single wait longer than thirty seconds, fully jittered.
-}
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  RetryPolicy
    { classifier = transient
    , backoff = exponential (milliseconds 100) <> limitRetries 3
    , jitter = FullJitter
    , maximumDelay = seconds 30
    }

{- | 'IOE' is here only for the jitter's entropy — effectful offers no
randomness effect, and any program that reaches the wire has 'IOE' anyway.
-}
withRetry :: (Concurrent :> es, Http :> es, IOE :> es) => RetryPolicy -> Eff es a -> Eff es a
withRetry policy = interpose_ $ \(Fetch req) -> attempt req (Attempt 1 mempty)
 where
  Backoff schedule = policy.backoff

  attempt req counter =
    fetch req >>= \exchange -> case planned req exchange counter of
      Nothing -> pure exchange
      Just wait -> do
        slept <- jittered policy.jitter wait
        delay slept
        attempt req Attempt{number = counter.number + 1, waited = counter.waited <> slept}

  planned req exchange counter = do
    guard $ replay req.body == Replayable
    verdict <- case classify policy.classifier req exchange of
      Deliver -> Nothing
      other   -> Just other
    scheduled <- schedule counter
    pure . min policy.maximumDelay $ case verdict of
      RetryAfter dictated -> dictated
      _                   -> scheduled

-- * Deciding whether
data Verdict
  = -- | This exchange is the answer; another attempt cannot improve it.
    Deliver
  | Retry
  | {- | The server named a delay, via @Retry-After@. Overrides the backoff's
    for this attempt, but not 'maximumDelay'.
    -}
    RetryAfter Duration
  deriving stock (Show, Eq)

{- | @'<>'@ tries the left classifier and falls through to the right on
'Deliver', so a specific rule shadows a general one: @'whenStatus' … '<>'
'transient'@.
-}
newtype Classifier = Classifier (Request -> Exchange -> Verdict)

instance Semigroup Classifier where
  Classifier a <> Classifier b = Classifier $ \req exchange -> case a req exchange of
    Deliver -> b req exchange
    verdict -> verdict

instance Monoid Classifier where
  mempty = never

classify :: Classifier -> Request -> Exchange -> Verdict
classify (Classifier decide) = decide

never :: Classifier
never = Classifier $ \_ _ -> Deliver

{- | Retry what a second attempt could plausibly fix: connections that failed
or timed out, sockets that died mid-exchange, and the statuses a server uses
to say "later" — 408, 425, 429, 500, 502, 503, 504. Honours @Retry-After@.

Ignores idempotency, so a POST that timed out after the server received it
will be sent twice. Use 'idempotentOnly' unless every endpoint you call is
safe to repeat.
-}
transient :: Classifier
transient = Classifier $ \_ -> \case
  Failed (InvalidUrl _) -> Deliver
  Failed (Unreachable content)
    | recoverable content -> Retry
    | otherwise -> Deliver
  Responded response
    | statusCode (responseStatus response) `elem` congested ->
      maybe Retry RetryAfter $ retryAfter response
    | otherwise -> Deliver
 where
  congested :: [Int]
  congested = [408, 425, 429, 500, 502, 503, 504]

  recoverable :: HttpExceptionContent -> Bool
  recoverable = \case
    ConnectionTimeout       -> True
    ConnectionFailure{}     -> True
    ResponseTimeout         -> True
    NoResponseDataReceived  -> True
    IncompleteHeaders       -> True
    ResponseBodyTooShort{}  -> True
    ProxyConnectException{} -> True
    InternalException{}     -> True
    _                       -> False

{- | 'transient', but for methods that are not safe to repeat (POST, PATCH,
and anything unrecognised) only where the request demonstrably never reached
the server. A 'ResponseTimeout' in particular is /not/ retried for those: the
server may have processed the request and merely been slow to answer.
-}
idempotentOnly :: Classifier
idempotentOnly = Classifier $ \req exchange ->
  if repeatable req.method || neverLanded exchange
    then classify transient req exchange
    else Deliver
 where
  repeatable :: Method -> Bool
  repeatable m =
    m `elem`
      [ methodGet
      , methodHead
      , methodPut
      , methodDelete
      , methodOptions
      , methodTrace
      ]

  neverLanded :: Exchange -> Bool
  neverLanded = \case
    Failed (Unreachable ConnectionTimeout)       -> True
    Failed (Unreachable ConnectionFailure{})     -> True
    Failed (Unreachable ProxyConnectException{}) -> True
    _                                            -> False

-- | @'whenStatus' ((429 ==) . 'statusCode') ('RetryAfter' ('seconds' 60))@
whenStatus :: (Status -> Bool) -> Verdict -> Classifier
whenStatus matches verdict = Classifier $ \_ -> \case
  Responded response | matches (responseStatus response) -> verdict
  _                                                      -> Deliver

{- | The delay a server asked for. Only RFC 7231's delta-seconds form; the
HTTP-date form cannot be resolved without reading a clock, and callers fall
back to the backoff's own schedule when this is 'Nothing'.
-}
retryAfter :: Response body -> Maybe Duration
retryAfter response = do
  raw <- lookup hRetryAfter (responseHeaders response)
  (count, rest) <- Char8.readInt (Char8.dropWhile isSpace raw)
  guard . Char8.all isSpace $ rest
  pure . seconds . fromIntegral $ count

-- * Deciding when

data Attempt = Attempt
  { number :: Int
  {- ^ Which retry is under consideration: 1 is the first retry, so the
  second time the request goes out.
  -}
  , waited :: Duration
  -- ^ Total slept across the retries already made.
  }
  deriving stock (Show, Eq)

{- | How long before the given attempt, and 'Nothing' to stop. @'<>'@ takes
the longer of two delays and gives up as soon as either side does, which
makes the ceilings ('limitRetries', 'limitTotalDelay') compose with the
curves ('exponential', 'fibonacci') without either knowing about the other.
-}
newtype Backoff = Backoff (Attempt -> Maybe Duration)

instance Semigroup Backoff where
  Backoff a <> Backoff b = Backoff $ \attempt -> max <$> a attempt <*> b attempt

instance Monoid Backoff where
  mempty = constant mempty

constant :: Duration -> Backoff
constant = Backoff . const . Just

-- | @base@, @2 * base@, @4 * base@, …
exponential :: Duration -> Backoff
exponential base = Backoff $ \attempt ->
  Just . flip scale base $ 2 ^^ max 0 (attempt.number - 1)

-- | @base@, @base@, @2 * base@, @3 * base@, @5 * base@, … — a gentler climb.
fibonacci :: Duration -> Backoff
fibonacci base = Backoff $ \attempt -> Just . flip scale base $ nth attempt.number
 where
  nth n
    | n <= 1 = 1
    | otherwise = go 1 1 n
  go current next n
    | n <= 1 = current
    | otherwise = go next (current + next) (n - 1)

-- | At most @n@ retries, so at most @n + 1@ sends.
limitRetries :: Int -> Backoff
limitRetries limit = Backoff $ \attempt -> guard (attempt.number <= limit) $> mempty

-- | Stop once this much has been spent sleeping, however many retries that took.
limitTotalDelay :: Duration -> Backoff
limitTotalDelay budget = Backoff $ \attempt -> guard (attempt.waited < budget) $> mempty

giveUp :: Backoff
giveUp = Backoff $ const Nothing

-- * Spreading the herd

data Jitter
  = NoJitter
  | {- | Uniform over @[0, d]@. Spreads a thundering herd best, at the cost of
    occasionally retrying almost immediately.
    -}
    FullJitter
  | -- | Uniform over @[d/2, d]@. Keeps a floor under the delay.
    EqualJitter
  deriving stock (Show, Eq)

jittered :: (IOE :> es) => Jitter -> Duration -> Eff es Duration
jittered NoJitter d    = pure d
jittered FullJitter d  = flip scale d <$> fraction 0 1
jittered EqualJitter d = flip scale d <$> fraction 0.5 1

fraction :: (IOE :> es) => Double -> Double -> Eff es Double
fraction lo hi = liftIO $ uniformRM (lo, hi) globalStdGen
