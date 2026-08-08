{-# LANGUAGE DerivingVia #-}
{- | An HTTP client as a dynamic effect with a single operation, so that every
cross-cutting concern — retries, auth, base URLs, observation — is an ordinary
@'Eff' es a -> 'Eff' es a@ built on 'Effectful.Dispatch.Dynamic.interpose'
rather than another field on the interpreter. See
"Effectful.Http.Retry" and "Effectful.Http.Middleware" for the
canonical ones.

Transport failures are values, not exceptions: 'runHttp' catches
'HttpException' at the wire and hands back a 'Failed' 'Exchange'. Middleware
therefore pattern matches on what went wrong instead of wrapping itself in
@catch@, which is what makes a retry loop readable. Non-2xx responses are not
failures at all — @checkResponse@ is left at http-client's default, so the
server's answer arrives as 'Responded' whatever its status, and deciding what
a status /means/ is the caller's or a middleware's job.

Requests are built variadically. 'collect' accumulates any number of
'Applies' arguments and stops at whatever the call site asks for:

@
'get' ('https' "api.example.com" '/:' "v1" '/:' "users")
  ('header' "Accept" "application/json")
  ('param' "page" "2")
  :: 'Eff' es ('Outcome' [User])
@

Ask for @'Eff' es 'Exchange'@ to see the raw response, or for @'Request'@ to
build one without sending it.
-}
module Effectful.Http (
  -- * The effect
  Http (..),
  Exchange (..),
  Failure (..),
  fetch,
  runHttp,

  -- * URLs
  Url (..),
  Scheme (..),
  Host (..),
  Port (..),
  PathSegment (..),
  http,
  https,
  withPort,
  (/:),
  render,
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
  Body (..),
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

  -- * Decoding a response
  Outcome (..),
  decodeOutcome,
) where

import Control.Exception                     (throwIO)
import Data.Aeson                            (FromJSON, ToJSON, eitherDecode', encode)
import Data.ByteString                       (ByteString)
import Data.ByteString.Builder               (Builder)
import Data.ByteString.Lazy                  (fromStrict)
import Data.ByteString.Lazy                  qualified as Lazy
import Data.Char                             (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import Data.Coerce                           (coerce)
import Data.Functor                          ((<&>))
import Data.Int                              (Int64)
import Data.String                           (IsString (..))
import Data.Text                             (Text)
import Data.Text                             qualified as Text
import Data.Text.Encoding                    (decodeUtf8, encodeUtf8)
import Data.Word                             (Word16)
import Effectful
import Effectful.Dispatch.Dynamic            (interpret_, send)
import Effectful.Exception                   (try)
import Effectful.Http.Duration               (Duration, toMicroseconds)
import GHC.Stack                             (HasCallStack)
import Network.HTTP.Client                   hiding (Request, path, port)
import Network.HTTP.Client                   qualified as Wire
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import Network.HTTP.Types
import Prelude                               hiding (head)

-- * The effect

{- | The one operation. Everything else in this package is either a way to
build the 'Request' or a way to intercept the operation.
-}
data Http :: Effect where
  Fetch :: Request -> Http m Exchange

type instance DispatchOf Http = Dynamic

-- | What came back — or did not.
data Exchange
  = Responded (Response Lazy.ByteString)
  | Failed Failure
  deriving stock (Show)

-- | Why no response arrived. Never a status code: a 503 is a 'Responded'.
data Failure
  = {- | The request reached the network stack and no usable response came
    back: refused, reset, TLS rejected, timed out.
    -}
    Unreachable HttpExceptionContent
  | {- | Nothing was ever sent, because the destination could not be named:
    a 'Host' that 'validHost' rejects, or a redirect to a @Location@ that
    http-client cannot parse.
    -}
    InvalidUrl Text
  deriving stock (Show)

fetch :: (HasCallStack, Http :> es) => Request -> Eff es Exchange
fetch = send . Fetch

-- * URLs

data Scheme = Http | Https
  deriving stock (Show, Eq)

newtype Host = Host Text
  deriving newtype (Show, Eq, IsString)

newtype Port = Port Word16
  deriving newtype (Show, Eq)

{- | One segment of a path, never containing a @\/@ of its own: 'render'
inserts the separators.

'Segment' is percent-encoded on the way out and is what a string literal
means. 'Encoded' is passed through untouched, for the callers that must
decide the bytes themselves — an API that signs the URL it receives, such as
S3's SigV4, rejects the request when the encoding it would have chosen and
the one on the wire disagree, and the encoding here is http-types', which
leaves @:\@&=+$,@ unescaped.
-}
data PathSegment
  = Segment Text
  | Encoded Text
  deriving stock (Show, Eq)

instance IsString PathSegment where
  fromString = Segment . Text.pack

data Url = Url
  { scheme :: Scheme
  , host   :: Host
  , port   :: Maybe Port
  , path   :: [PathSegment]
  }
  deriving stock (Show, Eq)

{- | Extend the left URL's path with the right one's, keeping the left
authority. This is @'/:'@ generalised to many segments, and the direction
'Effectful.Http.Middleware.withBaseUrl' needs.
-}
instance Semigroup Url where
  a <> b = a{path = a.path <> b.path}

http :: Host -> Url
http h = Url Http h Nothing []

https :: Host -> Url
https h = Url Https h Nothing []

withPort :: Word16 -> Url -> Url
withPort p u = u{port = Just (Port p)}

(/:) :: Url -> PathSegment -> Url
u /: segment = u{path = u.path <> [segment]}

infixl 5 /:

render :: Url -> Text
render Url{scheme, host, port, path} =
  prefix scheme <> coerce host <> maybe "" renderPort port <> renderPath path
 where
  prefix Http  = "http://"
  prefix Https = "https://"
  renderPort (Port p) = ":" <> Text.pack (show p)

renderPath :: [PathSegment] -> Text
renderPath = foldMap segment
 where
  segment (Segment raw) = "/" <> decodeUtf8 (urlEncode False (encodeUtf8 raw))
  segment (Encoded raw) = "/" <> raw


-- * Requests

data Request = Request
  { method    :: Method
  , url       :: Url
  , headers   :: [Header]
  , query     :: Query
  , body      :: Body
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
request method url = Request method url [] [] NoBody DefaultTimeout (FollowUpTo 10)

header :: HeaderName -> ByteString -> Header
header = (,)

contentType :: ByteString -> Header
contentType = header hContentType

param :: ByteString -> ByteString -> Query
param k v = [(k, Just v)]

-- * Request bodies

data Body
  = NoBody
  | Bytes Lazy.ByteString
  | Built Int64 Builder
  | Json Lazy.ByteString
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

replay :: Body -> Replay
replay = \case
  Streamed{}        -> OneShot
  StreamedChunked{} -> OneShot
  _                 -> Replayable

jsonBody :: (ToJSON a) => a -> Body
jsonBody = Json . encode

strictBody :: ByteString -> Body
strictBody = Bytes . fromStrict

-- * Assembling arguments

class Applies a where
  applyArg :: a -> Request -> Request

instance Applies Body where applyArg body r = r{body}
instance Applies Header where applyArg h r = r{headers = h : r.headers}
instance Applies [Header] where applyArg hs r = r{headers = hs <> r.headers}
instance Applies Query where applyArg q r = r{query = q <> r.query}
instance Applies Timeout where applyArg timeout r = r{timeout}
instance Applies Redirects where applyArg redirects r = r{redirects}

{- | Accumulates 'Applies' arguments until the call site asks for a result.
The instances below are the terminators: a 'Request' to hold onto, the raw
'Exchange', or a decoded 'Outcome'.
-}
class HttpArgs r where
  collect :: Request -> r

instance (Applies a, HttpArgs r) => HttpArgs (a -> r) where
  collect r arg = collect (applyArg arg r)

instance HttpArgs Request where
  collect = id

instance (Http :> es) => HttpArgs (Eff es Exchange) where
  collect = fetch

instance (Http :> es, FromJSON a) => HttpArgs (Eff es (Outcome a)) where
  collect = fmap decodeOutcome . fetch

-- * Verbs

get, post, put, patch, delete, head, options :: (HttpArgs r) => Url -> r
get = collect . request methodGet
post = collect . request methodPost
put = collect . request methodPut
patch = collect . request methodPatch
delete = collect . request methodDelete
head = collect . request methodHead
options = collect . request methodOptions

-- * Decoding a response

data Outcome a
  = Succeeded a
  | -- | 2xx, but the body is not what the decoder expected; its complaint.
    Undecodable (Response Lazy.ByteString) Text
  | -- | The server answered, outside 2xx.
    Rejected (Response Lazy.ByteString)
  | Unanswered Failure
  deriving stock (Show, Functor)

decodeOutcome :: (FromJSON a) => Exchange -> Outcome a
decodeOutcome = \case
  Failed failure -> Unanswered failure
  Responded response
    | statusIsSuccessful (responseStatus response) ->
        either (Undecodable response . Text.pack) Succeeded . eitherDecode' $ responseBody response
    | otherwise -> Rejected response

-- * Running

runHttp :: (IOE :> es) => Manager -> Eff (Http : es) a -> Eff es a
runHttp manager = interpret_ $ \(Fetch req) ->
  try @HttpException (liftIO $ buildRequest req >>= flip httpLbs manager) <&> \case
    Right response                        -> Responded response
    Left (HttpExceptionRequest _ content) -> Failed (Unreachable content)
    Left (InvalidUrlException _ reason)   -> Failed (InvalidUrl (Text.pack reason))

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
          , Wire.port = maybe implicitPort (\(Port p) -> fromIntegral p) req.url.port
          , Wire.path = encodeUtf8 . nonEmptyPath $ renderPath req.url.path
          , Wire.requestHeaders = req.headers
          , Wire.queryString = renderQuery True req.query
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

{- | Whether a 'Host' can stand as the authority on its own. A registered
name may carry only unreserved characters, and an IPv6 literal must be
bracketed. Everything else — @\@@, @\/@, @#@, @?@, whitespace, CR and LF —
would change which server the request reaches or what the request line says,
so it is rejected rather than escaped.

Punycode an internationalised domain before it gets here; the Host header
carries no encoding of its own.
-}
validHost :: Text -> Bool
validHost raw = case Text.uncons raw of
  Nothing          -> False
  Just ('[', rest) -> bracketed rest
  _                -> Text.all registered raw
 where
  bracketed rest = case Text.unsnoc rest of
    Just (inner, ']') -> not (Text.null inner) && Text.all ipv6 inner
    _                 -> False
  ipv6 c = isHexDigit c || c == ':' || c == '.'
  registered c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~" :: [Char])

wireTimeout :: Timeout -> ResponseTimeout
wireTimeout = \case
  DefaultTimeout -> responseTimeoutDefault
  NoTimeout      -> responseTimeoutNone
  TimeoutAfter d -> responseTimeoutMicro . fromIntegral $ toMicroseconds d

wireRedirects :: Redirects -> Int
wireRedirects = \case
  NoRedirects  -> 0
  FollowUpTo n -> max 0 n

applyBody :: Body -> Wire.Request -> IO Wire.Request
applyBody = \case
  NoBody -> pure
  Bytes payload -> pure . carrying (RequestBodyLBS payload)
  Built len builder -> pure . carrying (RequestBodyBuilder len builder)
  Json payload ->
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
