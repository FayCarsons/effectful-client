module ClientSpec (spec) where

import Control.Monad             (void)
import Data.List.NonEmpty        (NonEmpty (..))
import Data.Maybe                (fromMaybe, listToMaybe)
import Effectful
import Effectful.Http
import Effectful.Http.Middleware
import Network.HTTP.Client       (HttpExceptionContent (ConnectionTimeout))
import Network.HTTP.Types        (methodGet, methodPost)
import Prelude                   hiding (head)
import Scripted                  (responded, runScripted, unreachable)
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

  describe "the variadic builder" $ do
    it "collects headers and query parameters onto a request" $ do
      let built :: Request
          built = get (https "example.com") (header "Accept" "application/json") (param "page" "2")
      built.method `shouldBe` methodGet
      built.headers `shouldBe` [("Accept", "application/json")]
      built.query `shouldBe` [("page", Just "2")]

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

  describe "decodeOutcome" $ do
    it "decodes a 2xx body" $
      decodeOutcome @[Int] (responded 200 [] "[1,2,3]") `shouldSatisfy` \case
        Succeeded xs -> xs == [1, 2, 3]
        _            -> False

    it "separates a body it cannot read from one the server refused" $ do
      decodeOutcome @[Int] (responded 200 [] "not json") `shouldSatisfy` \case
        Undecodable{} -> True
        _             -> False
      decodeOutcome @[Int] (responded 404 [] "gone") `shouldSatisfy` \case
        Rejected{} -> True
        _          -> False

    it "reports a request that never got an answer" $
      decodeOutcome @[Int] (unreachable ConnectionTimeout) `shouldSatisfy` \case
        Unanswered (Unreachable ConnectionTimeout) -> True
        _                                          -> False

  describe "middleware" $ do
    it "applies the first of a stack closest to the program" $ do
      req <-
        sent
          (mconcat [Middleware (withPathPrefix ["inner"]), Middleware (withPathPrefix ["outer"])])
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
sent :: Middleware '[Http, IOE] -> IO Request
sent stack =
  fmap (fromMaybe (error "no request was made") . listToMaybe . snd)
    . runEff
    . runScripted (responded 200 [] "{}" :| [])
    . apply stack
    $ void asking
 where
  asking :: Eff '[Http, IOE] Exchange
  asking = get (url' /: "users") (header "Accept" "application/json")
