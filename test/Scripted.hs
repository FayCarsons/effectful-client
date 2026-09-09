{- | An 'Http' interpreter that answers from a canned script and records what
was asked, so retry and middleware behaviour can be pinned down without a
socket. This is the point of the effect being dynamic.

A scripted answer is read with 'readBody' exactly as 'Effectful.Http.runHttp'
reads a real one, so a script exercises decoding too: ask for a 'Json' body
and a scripted 4xx throws 'Rejected' here just as it would over the wire.
-}
module Scripted (
  Exchange (..),
  runScripted,
  rawResponse,
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
import Effectful.Error.Static       (Error, throwError)
import Effectful.Http               (Http (..), Request, ResponseBody (readBody))
import Effectful.Http               qualified as Http
import Effectful.State.Static.Local (State, get, put, runState)
import Network.HTTP.Client          (HttpExceptionContent)
import Network.HTTP.Client.Internal qualified as Wire
import Network.HTTP.Types           (Header, http11, mkStatus)

-- | One scripted answer: a wire-level response, or a connection that never landed.
data Exchange
  = Responded (Wire.Response Lazy.ByteString)
  | Failed HttpExceptionContent

{- | Serve the script in order, repeating its last entry once exhausted, and
hand back every 'Request' that was made in the order it was made.
-}
runScripted ::
  (Error Http.Error :> es) =>
  NonEmpty Exchange ->
  Eff (Http : es) a ->
  Eff es (a, [Request])
runScripted script action =
  second (reverse . snd) <$> reinterpret_ (runState (script, [])) serve action

serve ::
  (Error Http.Error :> es, State (NonEmpty Exchange, [Request]) :> es) =>
  Http m a ->
  Eff es a
serve (Fetch req) = do
  (remaining, seen) <- get
  put (advance remaining, req : seen)
  case NonEmpty.head remaining of
    Failed content     -> throwError $ Http.Unreachable content
    Responded response -> either throwError (\value -> pure $ value <$ response) $ readBody response
 where
  advance (next :| rest) = fromMaybe [next] (nonEmpty rest)

rawResponse :: Int -> [Header] -> Lazy.ByteString -> Wire.Response Lazy.ByteString
rawResponse code headers body =
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

responded :: Int -> [Header] -> Lazy.ByteString -> Exchange
responded code headers = Responded . rawResponse code headers

unreachable :: HttpExceptionContent -> Exchange
unreachable = Failed
