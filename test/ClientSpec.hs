{-# LANGUAGE NoOverloadedLists #-}
module ClientSpec (spec) where

import Control.Monad             (void)
import Data.Aeson                (decode, encode)
import Data.ByteString.Lazy      qualified as Lazy
import Data.Either               (isLeft)
import Data.Foldable             (for_)
import Data.List.NonEmpty        (NonEmpty (..))
import Data.Maybe                (fromMaybe, listToMaybe)
import Data.Text                 (Text)
import Effectful
import Effectful.Error.Static    (runErrorNoCallStack)
import Effectful.Error.Static    qualified as Eff
import Effectful.Http
import Effectful.Http.Middleware
import Network.HTTP.Client       (Response, defaultManagerSettings, newManager)
import Network.HTTP.Types        (methodGet, methodPost)
import Prelude                   hiding (head)
import Scripted                  (rawResponse, responded, runScripted)
import Test.Hspec

spec :: Spec
spec = do
  describe "Url" $ do
    it "renders scheme, host and path" $
      render (https "example.com" /: "v1" /: "users") `shouldBe` "https://example.com/v1/users"

    it "percent-encodes path segments" $
      render (https "example.com" /: "a b") `shouldBe` "https://example.com/a%20b"

    it "renders an explicit port" $
      render (withPort 8080 $ http "localhost" /: "health")
        `shouldBe` "http://localhost:8080/health"

    it "keeps the left authority when concatenated" $
      render ((https "api.example.com" /: "v1") <> (http "ignored" /: "users"))
        `shouldBe` "https://api.example.com/v1/users"

    it "renders its query string" $
      render (https "example.com" /: "users" /? [("page", Just "2")])
        `shouldBe` "https://example.com/users?page=2"

  describe "validHost" $ do
    it "accepts registered names and IPv6 literals" $
      for_ ["example.com", "localhost", "a-b.c_d~e", "127.0.0.1", "[::1]", "[fe80::1]"] (`shouldSatisfy` validHost)


    {- Each of these was previously rendered into a URL string and handed back
    to parseRequest, which read it as an authority again. The first sent the
    request to attacker.com. -}
    it "rejects anything that could be re-read as a different destination" $
      for_
        [ "evil.com@attacker.com"
        , "example.com/../../etc"
        , "example.com#fragment"
        , "example.com?q=1"
        , "example.com:8080"
        , "example.com\r\nX-Injected: 1"
        , "example com"
        , ""
        , "["
        , "[]"
        ]
        (`shouldNotSatisfy` validHost)

  describe "parseUrl" $ do
    {- Every parsed segment is 'Encoded': what arrived percent-encoded has to
    render back out exactly as it came, not escaped a second time. -}
    it "reads every part of an authority" $
      parseUrl "http://localhost:8080/v1/users?page=2"
        `shouldBe` Right
          (withPort 8080 (http "localhost") /: Encoded "v1" /: Encoded "users" /? [("page", Just "2")])

    it "reads a dotted name, a hyphenated one and an IPv6 literal" $
      for_ ["api.example.com", "my-service.internal", "[fe80::1]"] \host ->
        fmap (.host) (parseUrl $ "https://" <> host) `shouldBe` Right (Host host)

    it "leaves an escape as it found it, rather than encoding it again" $
      parseUrl "https://example.com/a%20b" `shouldBe` Right (https "example.com" /: Encoded "a%20b")

    it "round-trips whatever render writes" $
      for_
        [ "https://example.com"
        , "https://example.com/"
        , "http://127.0.0.1:5432/db/"
        , "https://api.example.com/v1/a%20b/c?q=a%2Bb&flag&empty="
        , "https://[::1]:8443/health"
        ]
        \raw -> fmap render (parseUrl raw) `shouldBe` Right raw


    it "takes a query apart the way a query is written" $
      fmap (.query) (parseUrl "https://example.com/?a=1&b=2&c")
        `shouldBe` Right [("a", Just "1"), ("b", Just "2"), ("c", Nothing)]

    it "keeps an implicit port implicit" $
      fmap (.port) (parseUrl "https://example.com") `shouldBe` Right Nothing

    {- Each of these would otherwise reach 'render' as a URL that names a
    different destination, or none at all. -}
    it "refuses what a Url cannot say" $
      for_
        [ "ftp://example.com"
        , "example.com/v1"
        , "https://"
        , "https://evil.com@attacker.com"
        , "https://example.com#fragment"
        , "https://example.com:0"
        , "https://example.com:99999"
        , "https://example.com:99999999999999999999"
        , "https://example.com:/v1"
        , "https://example.com/a b"
        , "https://example.com/a%2"
        , "https://example.com/a%zz"
        , "https://exa mple.com"
        , "https://example.com\r\nX-Injected: 1"
        , "https://[::1/health"
        , "https://ünicode.example.com"
        ]
        \raw -> parseUrl raw `shouldSatisfy` isLeft


    it "decodes from JSON and encodes back" $ do
      decode "\"https://example.com/v1?a=1\""
        `shouldBe` Just (https "example.com" /: Encoded "v1" /? [("a", Just "1")])
      encode (https "example.com" /: "v1") `shouldBe` "\"https://example.com/v1\""

  describe "a request that cannot name its destination" $
    it "fails as a value, before any connection is attempted" $ do
      manager <- newManager defaultManagerSettings
      outcome <-
        runEff . runErrorNoCallStack @Error . runHttp manager . fetch @Lazy.ByteString $
          request methodGet (https "evil.com@attacker.com" /: "users")
      outcome `shouldSatisfy` \case
        Left (InvalidUrl _) -> True
        _                   -> False

  describe "the variadic builder" $ do
    it "collects headers and query parameters onto a request" $ do
      let built :: Request
          built = get (https "example.com") (header "Accept" "application/json") (param "page" "2")
      built.method `shouldBe` methodGet
      built.headers `shouldBe` [("Accept", "application/json")]
      built.url.query `shouldBe` [("page", Just "2")]

    it "keeps headers and query parameters in call-site order" $ do
      let built :: Request
          built =
            get
              (https "example.com" /: "u")
              (header "A" "1")
              (header "B" "2")
              (param "p" "1")
              (param "q" "2")
      built.headers `shouldBe` [("A", "1"), ("B", "2")]
      built.url.query `shouldBe` [("p", Just "1"), ("q", Just "2")]

    it "gives each verb its method" $ do
      (post url' :: Request).method `shouldBe` methodPost
      (head url' :: Request).method `shouldBe` "HEAD"
      (delete url' :: Request).method `shouldBe` "DELETE"

  describe "replay" $ do
    it "treats a drained popper as one-shot" $ do
      replay (Streamed 0 ($ pure "")) `shouldBe` OneShot
      replay (StreamedChunked ($ pure "")) `shouldBe` OneShot

    it "treats a reopened file as replayable" $
      replay (FromFile "/etc/hosts") `shouldBe` Replayable

  describe "ResponseBody" $ do
    it "reads the bytes verbatim, whatever the status" $
      readBody @Lazy.ByteString (rawResponse 404 [] "gone") `shouldSatisfy` \case
        Right "gone" -> True
        _            -> False

    it "reads a well-formed body as Text" $
      readBody @Text (rawResponse 200 [] "hello") `shouldSatisfy` \case
        Right "hello" -> True
        _             -> False

    it "reports invalid UTF-8 as Unreadable, whatever the status" $
      readBody @Text (rawResponse 500 [] "\xff") `shouldSatisfy` \case
        Left (Unreadable _ _) -> True
        _                     -> False

    it "discards the body for ()" $
      readBody @() (rawResponse 500 [] "whatever") `shouldSatisfy` \case
        Right () -> True
        _        -> False

    describe "Json" $ do
      it "decodes a 2xx body" $
        readBody @(Json [Int]) (rawResponse 200 [] "[1,2,3]") `shouldSatisfy` \case
          Right (Json [1, 2, 3]) -> True
          _                      -> False

      it "reports a decode failure as Unreadable" $
        readBody @(Json [Int]) (rawResponse 200 [] "not json") `shouldSatisfy` \case
          Left (Unreadable _ _) -> True
          _                     -> False

      it "refuses a non-2xx response outright, without trying to decode" $
        readBody @(Json [Int]) (rawResponse 404 [] "gone") `shouldSatisfy` \case
          Left (Rejected _) -> True
          _                 -> False

  describe "middleware" $ do
    it "applies the first of a stack closest to the program" $ do
      req <-
        sent (mconcat [Middleware (withPathPrefix ["inner"]), Middleware (withPathPrefix ["outer"])])
      req.url.path `shouldBe` ["outer", "inner", "users"]

    it "lets withHeaders replace what the request set" $ do
      req <- sent (Middleware (withHeaders [("Accept", "text/plain")]))
      req.headers `shouldBe` [("Accept", "text/plain")]

    it "lets withDefaultHeaders defer to what the request set" $ do
      req <- sent (Middleware (withDefaultHeaders [("Accept", "text/plain")]))
      req.headers `shouldBe` [("Accept", "application/json")]

    it "redirects every request at a base URL" $ do
      req <- sent (Middleware (withBaseUrl (http "localhost" /: "stub")))
      render req.url `shouldBe` "http://localhost/stub/users"

    it "adds a bearer token" $ do
      req <- sent (Middleware (withBearerAuth "s3cret"))
      lookup "Authorization" req.headers `shouldBe` Just "Bearer s3cret"

    it "encodes basic credentials" $ do
      req <- sent (Middleware (withBasicAuth "aladdin" "opensesame"))
      lookup "Authorization" req.headers `shouldBe` Just "Basic YWxhZGRpbjpvcGVuc2VzYW1l"

url' :: Url
url' = https "example.com"

{- | The request that reached the wire after @stack@ shaped it. The program
asks for @\/users@ and names its own @Accept@, so both the overriding and the
deferring header middleware have something to act on.
-}
sent :: Middleware '[Http, Eff.Error Error, IOE] -> IO Request
sent stack = do
  outcome <-
    runEff
      . runErrorNoCallStack @Error
      . runScripted (responded 200 [] "{}" :| [])
      . apply stack
      $ void asking
  case outcome of
    Right (_, requests) -> pure . fromMaybe (error "no request was made") $ listToMaybe requests
    Left err            -> error $ "unexpected Error: " <> show err
 where
  asking :: Eff '[Http, Eff.Error Error, IOE] (Response Lazy.ByteString)
  asking = get (url' /: "users") (header "Accept" "application/json")
