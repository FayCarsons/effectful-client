module RetrySpec (spec) where

import Control.Monad           (void)
import Data.ByteString         (ByteString)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Maybe              (isJust)
import Data.Text               (Text)
import Data.Text               qualified as Text
import Effectful
import Effectful.Concurrent    (runConcurrent)
import Effectful.Http          (Body (NoBody, Streamed), Exchange (..), Request (..), fetch, https, request)
import Effectful.Http.Duration (Duration, milliseconds, seconds)
import Effectful.Http.Retry
import Hedgehog                (Gen, PropertyT, forAll, forAllWith, (===))
import Hedgehog.Gen            qualified as Gen
import Hedgehog.Range          qualified as Range
import Network.HTTP.Client     (HttpExceptionContent (..))
import Network.HTTP.Types      (hRetryAfter, methodGet, methodPost, statusCode)
import Scripted                (responded, runScripted, unreachable)
import System.Timeout          (timeout)
import Test.Hspec
import Test.Hspec.Hedgehog     (hedgehog)

spec :: Spec
spec = do
  describe "withRetry" $ do
    it "stops once the backoff gives up" $
      sends (immediate transient 2) (get' NoBody) (busy :| []) `shouldReturn` 3

    it "makes a single request when the first answer is good" $
      sends (immediate transient 5) (get' NoBody) (ok :| []) `shouldReturn` 1

    it "stops as soon as an attempt succeeds" $
      sends (immediate transient 5) (get' NoBody) (busy :| [busy, ok]) `shouldReturn` 3

    it "makes a single request when the classifier never retries" $
      sends (immediate never 5) (get' NoBody) (busy :| []) `shouldReturn` 1

    it "refuses to replay a one-shot body, whatever the classifier says" $
      sends (immediate transient 5) (post' streaming) (busy :| []) `shouldReturn` 1

    it "retries a replayable body on the same request" $
      sends (immediate transient 2) (post' NoBody) (busy :| []) `shouldReturn` 3

    it "clamps a server's Retry-After to maximumDelay" $ do
      let impatient = (immediate transient 1){maximumDelay = milliseconds 10}
      finishes (sends impatient (get' NoBody) (busyUntil "30" :| [])) `shouldReturn` Just 2

  describe "idempotentOnly" $ do
    it "retries a GET that timed out waiting for a response" $
      sends (immediate idempotentOnly 2) (get' NoBody) (timedOut :| []) `shouldReturn` 3

    it "will not retry a POST that may already have been processed" $
      sends (immediate idempotentOnly 2) (post' NoBody) (timedOut :| []) `shouldReturn` 1

    it "retries a POST that never reached the server" $
      sends (immediate idempotentOnly 2) (post' NoBody) (refused :| []) `shouldReturn` 3

    it "will not retry a POST that came back 503" $
      sends (immediate idempotentOnly 2) (post' NoBody) (busy :| []) `shouldReturn` 1

  describe "transient" $ do
    it "reads the delay a server asks for" $
      classify transient (get' NoBody) (busyUntil "3") `shouldBe` RetryAfter (seconds 3)

    it "falls back to the backoff when Retry-After is a date" $
      classify transient (get' NoBody) (busyUntil "Wed, 21 Oct 2015 07:28:00 GMT")
        `shouldBe` Retry

    it "delivers a response the server meant" $
      classify transient (get' NoBody) ok `shouldBe` Deliver

    it "delivers a 404, which no retry can fix" $
      classify transient (get' NoBody) (responded 404 [] "") `shouldBe` Deliver

  describe "Classifier composition" $
    it "lets a specific rule shadow the general one" $ do
      let policy = whenStatus ((409 ==) . statusCode) Retry <> transient
      classify policy (get' NoBody) (responded 409 [] "") `shouldBe` Retry
      classify policy (get' NoBody) (responded 503 [] "") `shouldBe` Retry
      classify policy (get' NoBody) (responded 404 [] "") `shouldBe` Deliver

  describe "retryAfter" $ do
    it "reads delta-seconds" $
      retryAfterOf "7" `shouldBe` Just (seconds 7)

    it "tolerates surrounding whitespace" $
      retryAfterOf "  7  " `shouldBe` Just (seconds 7)

    it "declines the HTTP-date form" $
      retryAfterOf "Wed, 21 Oct 2015 07:28:00 GMT" `shouldBe` Nothing

    it "declines trailing junk" $
      retryAfterOf "7 seconds" `shouldBe` Nothing

  describe "the Backoff monoid" $ do
    it "is associative" $ hedgehog $ do
      (a, b, c) <- (,,) <$> anyBackoff <*> anyBackoff <*> anyBackoff
      attempt <- forAll genAttempt
      schedule ((a <> b) <> c) attempt === schedule (a <> (b <> c)) attempt

    it "has mempty as an identity" $ hedgehog $ do
      a <- anyBackoff
      attempt <- forAll genAttempt
      schedule (mempty <> a) attempt === schedule a attempt
      schedule (a <> mempty) attempt === schedule a attempt

    it "gives up as soon as either side does" $ hedgehog $ do
      a <- anyBackoff
      attempt <- forAll genAttempt
      schedule (a <> giveUp) attempt === Nothing
      schedule (giveUp <> a) attempt === Nothing

    it "takes the longer of two delays" $ hedgehog $ do
      (a, b) <- forAll $ (,) <$> genDuration <*> genDuration
      attempt <- forAll genAttempt
      schedule (constant a <> constant b) attempt === Just (max a b)

  describe "Backoff limits" $ do
    it "limitRetries admits exactly its budget" $ hedgehog $ do
      limit <- forAll $ Gen.int (Range.linear 0 10)
      attempt <- forAll genAttempt
      isJust (schedule (limitRetries limit) attempt) === (attempt.number <= limit)

    it "limitTotalDelay stops once the budget is spent" $ hedgehog $ do
      budget <- forAll genDuration
      attempt <- forAll genAttempt
      isJust (schedule (limitTotalDelay budget) attempt) === (attempt.waited < budget)

    it "exponential never shrinks" $ hedgehog $ do
      base <- forAll genDuration
      n <- forAll $ Gen.int (Range.linear 1 20)
      let step k = schedule (exponential base) (Attempt k mempty)
      True === (step n <= step (n + 1))

  describe "Jitter" $
    it "keeps the delay within the schedule" $
      -- Both jitters only ever shorten a wait, so a policy that never sleeps
      -- stays a policy that never sleeps.
      finishes (sends ((immediate transient 3){jitter = FullJitter}) (get' NoBody) (busy :| []))
        `shouldReturn` Just 4

-- * Running

{- | How many requests reached the wire, after @policy@ had its say. Delays
are pinned to zero so the suite stays fast.
-}
sends :: RetryPolicy -> Request -> NonEmpty Exchange -> IO Int
sends policy req script =
  fmap (length . snd)
    . runEff
    . runConcurrent
    . runScripted script
    $ withRetry policy (void (fetch req))

-- | 'Nothing' if the action slept when it should not have.
finishes :: IO a -> IO (Maybe a)
finishes = timeout 2_000_000

immediate :: Classifier -> Int -> RetryPolicy
immediate classifier retries =
  RetryPolicy
    { classifier
    , backoff = constant mempty <> limitRetries retries
    , jitter = NoJitter
    , maximumDelay = mempty
    }

-- * Fixtures

get' :: Body -> Request
get' body = (request methodGet (https "example.com")){body}

post' :: Body -> Request
post' body = (request methodPost (https "example.com")){body}

streaming :: Body
streaming = Streamed 0 ($ pure "")

ok, busy, timedOut, refused :: Exchange
ok = responded 200 [] "{}"
busy = responded 503 [] ""
timedOut = unreachable ResponseTimeout
refused = unreachable ConnectionTimeout

busyUntil :: ByteString -> Exchange
busyUntil after = responded 503 [(hRetryAfter, after)] ""

retryAfterOf :: ByteString -> Maybe Duration
retryAfterOf after = case busyUntil after of
  Responded response -> retryAfter response
  _                  -> Nothing

-- * Generators

schedule :: Backoff -> Attempt -> Maybe Duration
schedule (Backoff decide) = decide

genDuration :: Gen Duration
genDuration = milliseconds <$> Gen.int64 (Range.linear 0 10_000)

genAttempt :: Gen Attempt
genAttempt = Attempt <$> Gen.int (Range.linear 1 20) <*> genDuration

{- | 'Backoff' is a function, so it cannot be shown. Carry a description
alongside each one and let 'forAllWith' report that instead.
-}
anyBackoff :: PropertyT IO Backoff
anyBackoff = snd <$> forAllWith (Text.unpack . fst) genBackoff

genBackoff :: Gen (Text, Backoff)
genBackoff =
  Gen.choice
    [ describing "constant" constant <$> genDuration
    , describing "exponential" exponential <$> genDuration
    , describing "fibonacci" fibonacci <$> genDuration
    , describing "limitRetries" limitRetries <$> Gen.int (Range.linear 0 10)
    , describing "limitTotalDelay" limitTotalDelay <$> genDuration
    , pure ("giveUp", giveUp)
    , pure ("mempty", mempty)
    ]
 where
  describing name build argument = (name <> " " <> Text.pack (show argument), build argument)
