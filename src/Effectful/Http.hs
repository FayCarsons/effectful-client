{-# LANGUAGE DerivingVia #-}
{- | An HTTP client as a dynamic effect with a single operation, so that every
cross-cutting concern — retries, auth, base URLs, observation — is an ordinary
@'Eff' es a -> 'Eff' es a@ built on 'Effectful.Dispatch.Dynamic.interpose'
rather than another field on the interpreter. See
"Effectful.Http.Retry" and "Effectful.Http.Middleware" for the
canonical ones.

Failure is 'Effectful.Error.Static.Error', not a value threaded through the
return type: 'runHttp' throws 'Error' at the wire, and a middleware
built with 'Effectful.Error.Static.tryError' pattern matches on what went
wrong instead of wrapping itself in @catch@, which is what makes a retry
loop readable.

The response type is chosen at the call site, through 'ResponseBody': asking
for 'Data.ByteString.Lazy.ByteString' reads the wire verbatim, whatever the
status, while asking for a 'Json' value refuses a non-2xx response outright.
The status policy therefore lives with the reading, not with the effect.

@
data User = User {name :: 'Data.Text.Text'}
  deriving stock (Generic)
  deriving anyclass (FromJSON)
  deriving ResponseBody via (Json User)

'get' ('https' "api.example.com" '/:' "v1" '/:' "users")
  ('header' "Accept" "application/json")
  ('param' "page" "2")
  :: 'Eff' es ('Network.HTTP.Client.Response' ('Json' [User]))
@

Ask for @'Eff' es ('Network.HTTP.Client.Response' 'Data.ByteString.Lazy.ByteString')@ to see the raw
response, or for @'Request'@ to build one without sending it.

Requests are built variadically. 'collect' accumulates any number of
'Applies' arguments and stops at whatever the call site asks for.
-}
module Effectful.Http (
  -- * The effect
  Http (..),
  Error (..),
  Attempted (..),
  attempted,
  tried,
  fetch,
  runHttp,

  -- * URLs

  -- | Re-exported from "Effectful.Http.Url", which also carries 'renderPath'.
  Url (..),
  Scheme (..),
  Host (..),
  Port (..),
  PathSegment (..),
  http,
  https,
  withPort,
  (/:),
  (/?),
  render,
  parseUrl,
  validHost,

  -- * Requests
  Request (..),
  Timeout (..),
  Redirects (..),
  request,
  header,
  contentType,
  param,

  -- * Request bodies
  RequestBody (..),
  Replay (..),
  replay,
  jsonBody,
  strictBody,

  -- * Verbs
  get,
  post,
  put,
  patch,
  delete,
  head,
  options,

  -- * Assembling arguments
  HttpArgs (..),
  Applies (..),

  -- * Reading a response
  ResponseBody (..),
  Json (..),
  onlySuccess,
) where

import Control.Exception                     (handle, throwIO)
import Data.Aeson                            (FromJSON, ToJSON, eitherDecode', encode)
import Data.Bifunctor                        (first)
import Data.ByteString                       (ByteString)
import Data.ByteString.Builder               (Builder)
import Data.ByteString.Lazy                  (fromStrict)
import Data.ByteString.Lazy                  qualified as Lazy
import Data.Coerce                           (coerce)
import Data.Int                              (Int64)
import Data.Text                             (Text)
import Data.Text                             qualified as Text
import Data.Text.Encoding                    (decodeUtf8', encodeUtf8)
import Data.Word                             (Word16)
import Effectful
import Effectful.Dispatch.Dynamic            (interpret_, send)
import Effectful.Error.Static                (throwError, tryError)
import Effectful.Error.Static                qualified as ErrorEff
import Effectful.Exception                   (try)
import Effectful.Http.Duration               (Duration, toMicroseconds)
import Effectful.Http.Url
import GHC.Stack                             (HasCallStack)
import Network.HTTP.Client                   hiding (Request, RequestBody, parseUrl, path, port)
import Network.HTTP.Client                   qualified as Wire
import Network.HTTP.Client.Internal          (mWrapException, toHttpException)
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import Network.HTTP.Types
import Prelude                               hiding (head)

-- * The effect

{- | The one operation. Everything else in this package is either a way to
build the 'Request' or a way to intercept the operation.
-}
data Http :: Effect where
  Fetch :: (ResponseBody a) => Request -> Http m (Response a)

type instance DispatchOf Http = Dynamic

-- | Why a 'fetch' did not deliver what was asked for.
data Error
  = {- | The request reached the network stack and no usable response came
    back: refused, reset, TLS rejected, timed out.
    -}
    Unreachable HttpExceptionContent
  | {- | Nothing was ever sent, because the destination could not be named:
    a 'Host' that 'validHost' rejects, or a redirect to a @Location@ that
    http-client cannot parse.
    -}
    InvalidUrl Text
  | -- | The server answered outside 2xx, and the reading in question declines that.
    Rejected (Response Lazy.ByteString)
  | -- | The server answered, and the bytes were not what the reading expected.
    Unreadable (Response Lazy.ByteString) Text
  deriving stock (Show)

{- | Sending is all this asks for. Throwing 'Error' is an obligation of
the interpretation, not of the operation — 'runHttp' takes it on, and an
interpreter that answers from a script need not — so the constraint belongs
on 'runHttp' and on the middleware that catches, and not here.

The @forall@ is ordered so that @'fetch' \@Value@ picks the reading rather
than the effect row.
-}
fetch :: forall a es. (HasCallStack, Http :> es, ResponseBody a) => Request -> Eff es (Response a)
fetch = send . Fetch

{- | What a single attempt came to, for a retry 'Effectful.Http.Retry.Classifier'
or 'Effectful.Http.Middleware.withObserver' to see. 'Answered' carries only
the status and headers — never the body — so it states in the type that
neither may look at what the server sent back: a 503 reaches a classifier
identically whether the call site asked for raw bytes (arriving as a
success) or for JSON (arriving as a thrown 'Rejected').
-}
data Attempted
  = -- | Never left the process; no retry can help. 'InvalidUrl''s reason.
    Unsent Text
  | -- | Went out, nothing usable came back. 'Unreachable''s content.
    NoAnswer HttpExceptionContent
  | Answered Status [Header]
  deriving stock (Show)

{- | Run a 'fetch' and hand back how it went instead of unwinding, for the
middleware that has to look before it decides. The call stack
'Effectful.Error.Static.tryError' pairs with the error is dropped: a
'Classifier' has no use for it, and 'throwError' would attach a fresh one on
the way back out anyway.
-}
tried :: (ErrorEff.Error Error :> es) => Eff es a -> Eff es (Either Error a)
tried = fmap (first snd) . tryError

-- | Collapse a read 'fetch' down to what a retry or an observer may see.
attempted :: Either Error (Response a) -> Attempted
attempted = \case
  Left (Unreachable content)   -> NoAnswer content
  Left (InvalidUrl reason)     -> Unsent reason
  Left (Rejected response)     -> answered response
  Left (Unreadable response _) -> answered response
  Right response               -> answered response
 where
  answered response = Answered (responseStatus response) (responseHeaders response)

-- * Requests

data Request = Request
  { method    :: Method
  , url       :: Url
  , headers   :: [Header]
  , body      :: RequestBody
  , timeout   :: Timeout
  , redirects :: Redirects
  }

-- | How long to wait for the response headers, not for the whole body.
data Timeout
  = DefaultTimeout
  | NoTimeout
  | TimeoutAfter Duration
  deriving stock (Show, Eq)

data Redirects
  = NoRedirects
  | FollowUpTo Int
  deriving stock (Show, Eq)

-- | A bodiless request with http-client's defaults, ready for 'applyArg'.
request :: Method -> Url -> Request
request method url = Request method url [] NoBody DefaultTimeout (FollowUpTo 10)

header :: HeaderName -> ByteString -> Header
header = (,)

contentType :: ByteString -> Header
contentType = header hContentType

param :: ByteString -> ByteString -> Query
param k v = [(k, Just v)]

-- * Request bodies

data RequestBody
  = NoBody
  | Bytes Lazy.ByteString
  | Built Int64 Builder
  | JsonBody Lazy.ByteString
  | Form Query
  | {- | Replayable as long as the parts are: 'formDataBody' asks each part
    for its body afresh, so file- and byte-backed parts survive a retry.
    -}
    Multipart [Part]
  | -- | Reopened on every send, so unlike 'Streamed' this can be retried.
    FromFile FilePath
  | Streamed Int64 (GivesPopper ())
  | StreamedChunked (GivesPopper ())

{- | Whether a body can be sent twice. A 'GivesPopper' is drained by the first
send, so replaying one puts a truncated or empty body on the wire —
"Effectful.Http.Retry" consults this before retrying anything.
-}
data Replay = Replayable | OneShot
  deriving stock (Show, Eq)

replay :: RequestBody -> Replay
replay = \case
  Streamed{}        -> OneShot
  StreamedChunked{} -> OneShot
  _                 -> Replayable

jsonBody :: (ToJSON a) => a -> RequestBody
jsonBody = JsonBody . encode

strictBody :: ByteString -> RequestBody
strictBody = Bytes . fromStrict

-- * Assembling arguments

class Applies a where
  applyArg :: a -> Request -> Request

instance Applies RequestBody where applyArg body r = r{body}
instance Applies Header where applyArg h r = r{headers = r.headers <> [h]}
instance Applies [Header] where applyArg hs r = r{headers = r.headers <> hs}
instance Applies Query where applyArg q r = r{url = r.url{query = r.url.query <> q}}
instance Applies Timeout where applyArg timeout r = r{timeout}
instance Applies Redirects where applyArg redirects r = r{redirects}

{- | Accumulates 'Applies' arguments until the call site asks for a result.
The instances below are the terminators: a 'Request' to hold onto, or a
'fetch'ed and read 'Network.HTTP.Client.Response'.
-}
class HttpArgs r where
  collect :: Request -> r

instance (Applies a, HttpArgs r) => HttpArgs (a -> r) where
  collect r arg = collect (applyArg arg r)

instance HttpArgs Request where
  collect = id

instance (Http :> es, ResponseBody a) => HttpArgs (Eff es (Response a)) where
  collect = fetch

-- * Verbs

get, post, put, patch, delete, head, options :: (HttpArgs r) => Url -> r
get = collect . request methodGet
post = collect . request methodPost
put = collect . request methodPut
patch = collect . request methodPatch
delete = collect . request methodDelete
head = collect . request methodHead
options = collect . request methodOptions

-- * Reading a response

{- | How to read a 'Network.HTTP.Client.Response' body, and — because a
status is not meaningful without knowing what the caller wanted — what to do
about a status outside 2xx. Choosing @a@ at the call site chooses the policy:
'Data.ByteString.Lazy.ByteString' and 'Data.ByteString.ByteString' hand back
whatever came, 'Json' refuses a refusal.
-}
class ResponseBody a where
  readBody :: Response Lazy.ByteString -> Either Error a

instance ResponseBody Lazy.ByteString where
  readBody = Right . responseBody

instance ResponseBody ByteString where
  readBody = Right . Lazy.toStrict . responseBody

instance ResponseBody Text where
  readBody response =
    first (Unreadable response . Text.pack . show)
      . decodeUtf8'
      . Lazy.toStrict
      $ responseBody response

instance ResponseBody () where
  readBody _ = Right ()

{- | A JSON response, reached through @DerivingVia@ so a call site never
writes the wrapper itself:

@
data User = User {name :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)
  deriving ResponseBody via (Json User)
@

Refuses any status outside 2xx with 'Rejected' before it even tries to
decode — a store's error document is not the shape @a@ promises.
-}
newtype Json a = Json a
  deriving stock (Show)

instance (FromJSON a) => ResponseBody (Json a) where
  readBody = onlySuccess $ \response ->
    first (Unreadable response . Text.pack) . fmap Json . eitherDecode' $ responseBody response

{- | The status policy 'Json' and any reading like it wants: decline
everything outside 2xx before the reading gets a chance to run.
-}
onlySuccess ::
  (Response Lazy.ByteString -> Either Error a) ->
  Response Lazy.ByteString ->
  Either Error a
onlySuccess readSuccess response
  | statusIsSuccessful (responseStatus response) = readSuccess response
  | otherwise = Left $ Rejected response

-- * Running

{- | Read the wire once with @httpLbs@, then read the body the caller asked
for. 'Network.HTTP.Client.Response' is a 'Functor', so rebuilding the
response around the read body is @(<$ raw)@.

http-client applies the manager's exception wrapper only while it opens
the response. @httpLbs@ reads the body afterwards, so a connection reset or
a TLS failure in the middle of the body would escape unwrapped. 'exchange'
applies the same wrapper to the whole exchange, so a failure in the body
becomes 'Unreachable' exactly as one in the headers does. The manager
decides which exceptions are transport failures: @http-client-tls@ adds
TLS and DNS failures to http-client's 'IOException'.
-}
runHttp :: (IOE :> es, ErrorEff.Error Error :> es) => Manager -> Eff (Http : es) a -> Eff es a
runHttp manager = interpret_ $ \(Fetch req) ->
  try @HttpException (liftIO $ buildRequest req >>= exchange) >>= \case
    Left (HttpExceptionRequest _ content) -> throwError $ Unreachable content
    Left (InvalidUrlException _ reason)   -> throwError . InvalidUrl $ Text.pack reason
    Right raw                             -> either throwError (\value -> pure $ value <$ raw) $ readBody raw
 where
  exchange wire =
    handle (throwIO . toHttpException wire) . mWrapException manager wire $ httpLbs wire manager

{- | Assemble the wire request field by field. Nothing here round-trips
through a URL string, so a 'Host' cannot be re-read as userinfo, a port, or
the start of a path — the authority is whatever 'Url' says it is.
-}
buildRequest :: Request -> IO Wire.Request
buildRequest req
  | not (validHost rawHost) = throwIO $ InvalidUrlException (Text.unpack url) "invalid host"
  | otherwise =
      applyBody req.body $
        Wire.defaultRequest
          { Wire.method = req.method
          , Wire.secure = encrypted
          , Wire.host = encodeUtf8 rawHost
          , Wire.port = maybe implicitPort (fromIntegral @Word16 . coerce) req.url.port
          , Wire.path = encodeUtf8 . nonEmptyPath $ renderPath req.url.path
          , Wire.requestHeaders = req.headers
          , Wire.queryString = renderQuery True req.url.query
          , Wire.responseTimeout = wireTimeout req.timeout
          , Wire.redirectCount = wireRedirects req.redirects
          }
 where
  url = render req.url
  rawHost = coerce req.url.host
  encrypted = req.url.scheme == Https
  implicitPort = if encrypted then 443 else 80
  nonEmptyPath rendered
    | Text.null rendered = "/"
    | otherwise = rendered

wireTimeout :: Timeout -> ResponseTimeout
wireTimeout = \case
  DefaultTimeout -> responseTimeoutDefault
  NoTimeout      -> responseTimeoutNone
  TimeoutAfter d -> responseTimeoutMicro . fromIntegral $ toMicroseconds d

wireRedirects :: Redirects -> Int
wireRedirects = \case
  NoRedirects  -> 0
  FollowUpTo n -> max 0 n

applyBody :: RequestBody -> Wire.Request -> IO Wire.Request
applyBody = \case
  NoBody -> pure
  Bytes payload -> pure . carrying (RequestBodyLBS payload)
  Built len builder -> pure . carrying (RequestBodyBuilder len builder)
  JsonBody payload ->
    pure . carrying (RequestBodyLBS payload) . defaultContentType "application/json"
  Form params ->
    pure
      . carrying (RequestBodyBS $ renderQuery False params)
      . defaultContentType "application/x-www-form-urlencoded"
  Multipart parts -> formDataBody parts
  FromFile path -> pure . carrying (RequestBodyIO $ streamFile path)
  Streamed len popper -> pure . carrying (RequestBodyStream len popper)
  StreamedChunked popper -> pure . carrying (RequestBodyStreamChunked popper)
 where
  carrying payload req = req{Wire.requestBody = payload}

defaultContentType :: ByteString -> Wire.Request -> Wire.Request
defaultContentType ct req
  | any ((hContentType ==) . fst) req.requestHeaders = req
  | otherwise = req{Wire.requestHeaders = (hContentType, ct) : req.requestHeaders}
