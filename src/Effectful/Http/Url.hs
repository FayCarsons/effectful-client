{-# LANGUAGE DefaultSignatures #-}
{- | The destination of a request, held apart rather than as a string.

A 'Url' names its authority in fields, so nothing that goes on the wire has
to be recovered from text a second time: 'Effectful.Http.runHttp' hands
http-client a host, a port and a path it never re-reads, and a 'Host' cannot
turn into userinfo, a port, or the start of a path along the way.

'render' writes the text form and 'parseUrl' reads it back. The two agree —
a parsed path segment is 'Encoded', so a round trip does not escape a @%@
that was already an escape — and both defer to 'validHost' on what may stand
as the authority.

A string literal is a host reached over HTTPS, '</>' appends a path
segment and '<?>' a query parameter. Segments and parameter values can be
anything with a 'UrlPiece' instance:

> "api.example.com" </> "users" </> userId <?> "lightMode" := True
-}
module Effectful.Http.Url
  ( Url (..)
  , Scheme (..)
  , Host (..)
  , Port (..)
  , PathSegment (..)
  , UrlPiece (..)
  , Param (..)
  , http
  , https
  , withPort
  , (</>)
  , segment
  , (<?>)
  , withQuery
  , render
  , renderPath
  , validHost
  , parseUrl
  ) where

import Data.Aeson         (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Char          (digitToInt, isDigit, isHexDigit)
import Data.Char          qualified as Char
import Data.Coerce        (coerce)
import Data.String        (IsString (..))
import Data.Text          (Text)
import Data.Text          qualified as Text
import Data.Int           (Int16, Int32, Int64, Int8)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy     qualified as Lazy
import Data.Time          (Day, UTCTime)
import Data.UUID.Types    (UUID)
import Data.Word          (Word16, Word32, Word64, Word8)
import Numeric.Natural    (Natural)
import FlatParse.Basic    (Parser, Result (..))
import FlatParse.Basic    qualified as Parse
import Network.HTTP.Types (Query, QueryItem, parseQuery, renderQuery, urlEncode)
import Web.HttpApiData    (ToHttpApiData (..))

data Scheme
  = Http
  | Https
  deriving stock (Show, Eq)

newtype Host = Host Text
  deriving newtype (Show, Eq, IsString)

newtype Port = Port Word16
  deriving newtype (Show, Eq)

{- | One segment of a path, never containing a @\/@ of its own: 'render'
inserts the separators.

'Segment' is percent-encoded on the way out, and is what a string literal
and '</>' produce. 'Encoded' is passed through untouched, for the callers that must
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
  , query  :: Query
  }
  deriving stock (Show, Eq)

{- | A host, reached over HTTPS on its default port: @"api.example.com"@ is
@'https' "api.example.com"@. Override either default with 'http' or
'withPort'.

The literal is a host and nothing more. Like every 'Host', it is checked by
'validHost' before anything is sent, so @"api.example.com\/v1"@ fails as
'Effectful.Http.InvalidUrl' instead of reaching the wrong server. Write
@"api.example.com" '</>' "v1"@.
-}
instance IsString Url where
  fromString = https . fromString

{- | Extend the left URL's path and query with the right one's, keeping the
left authority. This is @'</>'@ generalised to many segments, and the
direction 'Effectful.Http.Middleware.withBaseUrl' needs.
-}
instance Semigroup Url where
  a <> b = a{path = a.path <> b.path, query = a.query <> b.query}

http :: Host -> Url
http host = Url{scheme = Http, host, port = Nothing, path = [], query = []}

https :: Host -> Url
https host = Url{scheme = Https, host, port = Nothing, path = [], query = []}

withPort :: Word16 -> Url -> Url
withPort p u = u{port = Just (Port p)}

{- | What can stand as a path segment or a query parameter value, as the
text it renders to before percent-encoding.

A type with a 'ToHttpApiData' instance gets this one from it:

> newtype UserId = UserId Int
>   deriving newtype (ToHttpApiData)
>   deriving anyclass (UrlPiece)
-}
class UrlPiece a where
  urlPiece :: a -> Text
  default urlPiece :: (ToHttpApiData a) => a -> Text
  urlPiece = toUrlPiece

{- | 'Text', and what makes a string literal one: @"users"@ is ambiguous
until something fixes its type, and this is the only instance that matches
a type not yet known.

Every other instance is more specific, so this one never displaces them.
Three things it cannot do:

* resolve a numeric literal, since 'Text' has no 'Num': write
  @"page" ':=' (2 :: Int)@;
* resolve a string literal inside a function that is itself given
  @UrlPiece a@, because GHC will not choose between that constraint and
  this instance: write @("users" :: Text)@ there;
* name the missing instance: a type without one is reported as not being
  'Text'.

GHC 9.12 offers named defaults (@default UrlPiece (Text, Int)@) for exactly
this, but does not apply them to a class from another package, which is
every caller of this one.
-}
instance {-# INCOHERENT #-} (a ~ Text) => UrlPiece a where
  urlPiece = id

instance UrlPiece Lazy.Text
instance UrlPiece String
instance UrlPiece Bool
instance UrlPiece Int
instance UrlPiece Int8
instance UrlPiece Int16
instance UrlPiece Int32
instance UrlPiece Int64
instance UrlPiece Integer
instance UrlPiece Natural
instance UrlPiece Word
instance UrlPiece Word8
instance UrlPiece Word16
instance UrlPiece Word32
instance UrlPiece Word64
instance UrlPiece Double
instance UrlPiece Float
instance UrlPiece UUID
instance UrlPiece Day
instance UrlPiece UTCTime

{- | Append one path segment, rendered by 'urlPiece' and percent-encoded on
the way out, so a @\/@ in the value stays inside its segment.

> "api.example.com" </> "users" </> userId

To append bytes that are already encoded, extend 'path' with 'Encoded'.
-}
(</>) :: (UrlPiece a) => Url -> a -> Url
url </> piece = segment piece url

infixl 5 </>

-- | '</>' as a function, for a module where filepath's operator of the same name is in scope.
segment :: (UrlPiece a) => a -> Url -> Url
segment piece url = url{path = url.path <> [Segment $ urlPiece piece]}

{- | One query parameter, its value rendered by 'urlPiece'.

> "api.example.com" </> "search" <?> "q" := term <?> "page" := page
-}
data Param where
  (:=) :: (UrlPiece v) => Text -> v -> Param

infix 7 :=

-- | Append one query parameter, keeping whatever the URL already carried.
(<?>) :: Url -> Param -> Url
url <?> param = withQuery param url

infixl 5 <?>

-- | '<?>' as a function, for a module where parsec's operator of the same name is in scope.
withQuery :: Param -> Url -> Url
withQuery param url = url{query = url.query <> [queryItem param]}

-- | A 'Param' as http-types writes it.
queryItem :: Param -> QueryItem
queryItem (key := value) = (encodeUtf8 key, Just . encodeUtf8 $ urlPiece value)

render :: Url -> Text
render Url{scheme, host, port, path, query} =
  prefix scheme
    <> coerce host
    <> maybe "" renderPort port
    <> renderPath path
    <> decodeUtf8 (renderQuery True query)
 where
  prefix Http  = "http://"
  prefix Https = "https://"
  renderPort (Port p) = ":" <> Text.pack (show p)

renderPath :: [PathSegment] -> Text
renderPath = foldMap segment where
  segment (Segment raw) = "/" <> decodeUtf8 (urlEncode False (encodeUtf8 raw))
  segment (Encoded raw) = "/" <> raw

{- | Read back what 'render' writes.

Only what a 'Url' can say is accepted: @http@ or @https@, a host 'validHost'
agrees to, no userinfo and no fragment — the first cannot be represented and
the second is never sent, and dropping either silently would answer a
different destination than the one that was asked for.

Percent-escapes arrive already written, so every parsed segment is 'Encoded'
and a round trip through 'render' cannot escape a @%@ a second time. That
also makes ASCII the whole alphabet: punycode an internationalised host and
percent-encode the rest before handing the text over.
-}

parseUrl :: Text -> Either Text Url
parseUrl text
  | not $ Text.isAscii text = Left "url is not ASCII"
  | Text.null text = Left "empty url"
  | otherwise =
    case Parse.runParser urlParser $ encodeUtf8 text of
      OK url _ -> Right url
      Err why  -> Left why
      Fail     -> Left "malformed url"

instance FromJSON Url where
  parseJSON = withText "Url" \text ->
    if not $ Text.isAscii text then fail "url is not ASCII"
      else
        case Parse.runParser urlParser $ encodeUtf8 text of
          OK url _ -> pure url
          Err why  -> fail $ Text.unpack why
          Fail     -> fail "malformed url"

instance ToJSON Url where
  toJSON = String . render

urlParser :: Parser Text Url
urlParser = do
  scheme <- schemeParser
  host <- hostParser
  port <- Parse.optional $ $(Parse.char ':') *> portParser
  path <- Parse.many $ $(Parse.char '/') *> segmentParser
  query <- Parse.branch $(Parse.char '?') queryParser $ pure []
  Parse.takeRest >>= exhausted . decodeUtf8
  pure Url{..}
 where
  schemeParser = do
    name <- Parse.cut (spanOf schemeChar) "url has no scheme"
    Parse.cut $(Parse.string "://") $ "expected \"://\" after the scheme " <> name
    named $ Text.toLower name

  named "http"  = pure Http
  named "https" = pure Https
  named other   = Parse.err $ "unsupported scheme: " <> other

  hostParser = do
    hostText <- Parse.cut (Parse.branch $(Parse.char '[') bracketed $ spanOf regNameChar) "url has no host"
    addressed hostText

  bracketed = do
    inner <- spanOf ipv6Char
    Parse.cut $(Parse.char ']') "unterminated IPv6 literal"
    pure $ "[" <> inner <> "]"

  addressed raw
    | Text.any ('@' ==) raw = Parse.err "userinfo is not part of a url"
    | validHost raw = pure $ Host raw
    | otherwise = Parse.err $ "invalid host: " <> raw

  portParser = Parse.cut (spanOf isDigit) "expected a port number after \":\"" >>= numbered

  numbered digits
    | Text.length digits > 5 || value < 1 || value > 65535 =
        Parse.err $ "port out of range: " <> digits
    | otherwise = pure . Port $ fromIntegral value
   where
    value = Text.foldl' place 0 digits
    place acc digit = acc * 10 + digitToInt digit

  segmentParser = Encoded . decodeUtf8 <$> escapedSpan pathChar
  queryParser = parseQuery <$> escapedSpan queryChar

  exhausted leftover
    | Text.null leftover = pure ()
    | "#" `Text.isPrefixOf` leftover = Parse.err "a url fragment is never sent; drop it"
    | otherwise = Parse.err $ "unexpected input: " <> leftover

  spanOf accepts = decodeUtf8 <$> Parse.byteStringOf (Parse.skipSome $ Parse.satisfyAscii accepts)

  {- A run of already-encoded octets, kept verbatim. A bare % that no pair of
  hex digits follows would render back out as a different escape, so it is
  refused here rather than passed through. -}
  escapedSpan accepts =
    Parse.byteStringOf . Parse.skipMany $
      Parse.branch $(Parse.char '%') escape (Parse.skipSatisfyAscii accepts)
   where
    escape = Parse.cut (hexDigit *> hexDigit) "% must be followed by two hex digits"
    hexDigit = Parse.skipSatisfyAscii isHexDigit

  schemeChar c = Char.isAlphaNum c || c `elem` ("+-." :: [Char])
  regNameChar c = c `notElem` (":/?#" :: [Char])
  pathChar c = unreserved c || subDelim c || c `elem` (":@" :: [Char])
  queryChar c = pathChar c || c `elem` ("/?" :: [Char])
  subDelim c = c `elem` ("!$&'()*+,;=" :: [Char])

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
  _                -> Text.all unreserved raw
 where
  bracketed rest = case Text.unsnoc rest of
    Just (inner, ']') -> not (Text.null inner) && Text.all ipv6Char inner
    _                 -> False

ipv6Char :: Char -> Bool
ipv6Char c = isHexDigit c || c == ':' || c == '.'

unreserved :: Char -> Bool
unreserved c = Char.isAlphaNum c || c `elem` ("-._~" :: [Char])
