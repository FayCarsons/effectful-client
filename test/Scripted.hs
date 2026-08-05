{- | An 'Http' interpreter that answers from a canned script and records what
was asked, so retry and middleware behaviour can be pinned down without a
socket. This is the point of the effect being dynamic.
-}
module Scripted (
  runScripted,
  responded,
  unreachable,
) where

import Data.Bifunctor               (second)
import Data.ByteString.Lazy         qualified as Lazy
import Data.List.NonEmpty           (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty           qualified as NonEmpty
import Data.Maybe                   (fromMaybe)
import Effectful
import Effectful.Dispatch.Dynamic   (reinterpret_)
import Effectful.Http               (Exchange (..), Failure (..), Http (..), Request)
import Effectful.State.Static.Local (State, get, put, runState)
import Network.HTTP.Client          (HttpExceptionContent)
import Network.HTTP.Client.Internal qualified as Wire
import Network.HTTP.Types           (Header, http11, mkStatus)

{- | Serve the script in order, repeating its last entry once exhausted, and
hand back every 'Request' that was made in the order it was made.
-}
runScripted :: NonEmpty Exchange -> Eff (Http : es) a -> Eff es (a, [Request])
runScripted script action =
  second (reverse . snd) <$> reinterpret_ (runState (script, [])) serve action

serve :: (State (NonEmpty Exchange, [Request]) :> es) => Http m a -> Eff es a
serve (Fetch req) = do
  (remaining, seen) <- get
  put (advance remaining, req : seen)
  pure (NonEmpty.head remaining)
 where
  advance (next :| rest) = fromMaybe (next :| []) (nonEmpty rest)

responded :: Int -> [Header] -> Lazy.ByteString -> Exchange
responded code headers body =
  Responded
    Wire.Response
      { Wire.responseStatus = mkStatus code ""
      , Wire.responseVersion = http11
      , Wire.responseHeaders = headers
      , Wire.responseBody = body
      , Wire.responseCookieJar = Wire.createCookieJar []
      , Wire.responseClose' = Wire.ResponseClose (pure ())
      , Wire.responseOriginalRequest = Wire.defaultRequest
      , Wire.responseEarlyHints = []
      }

unreachable :: HttpExceptionContent -> Exchange
unreachable = Failed . Unreachable
