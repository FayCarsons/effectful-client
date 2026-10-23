{-# LANGUAGE DerivingVia #-}
{- | Cross-cutting request shaping for "Effectful.Http", and the
composition order every middleware in this package obeys.

Each one is an ordinary @'Eff' es a -> 'Eff' es a@, so @.@ composes them; the
'Middleware' newtype exists only so a list of them can be assembled at
runtime. Either way the rule is the same: __the first middleware listed is
the one closest to the program, and the last is closest to the wire__.

@
'Effectful.Http.runHttp' manager
  . 'apply'
      ('mconcat'
        [ 'Effectful.Http.Retry.withRetry' 'Effectful.Http.Retry.defaultRetryPolicy'
        , 'withBearerAuth' token -- re-signs each attempt
        , 'withObserver' record  -- sees every attempt
        ])
  $ program
@

That ordering matters more than it looks. Auth placed /after/ the retry, as
above, is re-applied on every attempt — which is what a signature over a
timestamp, or a token that may have been refreshed, requires. Auth placed
before it is computed once and replayed verbatim.
-}
module Effectful.Http.Middleware
  ( -- * Composing
    Middleware (..)
  , apply
    -- * Building your own
  , shaping
  , withObserver
    -- * Headers
  , withHeaders
  , withDefaultHeaders
  , withBearerAuth
  , withBasicAuth
  , withApiKey
  , withUserAgent
    -- * Addressing
  , withBaseUrl
  , withPathPrefix
  , withQuery
    -- * Transport
  , withTimeout
  , withRedirects
  ) where

import Data.ByteString            (ByteString)
import Data.ByteString.Base64     qualified as Base64
import Effectful
import Effectful.Dispatch.Dynamic (interpose_)
import Effectful.Error.Static     (Error, throwError)
import Effectful.Http             (Attempted, Http (..), PathSegment, Redirects, Request (..), Timeout (TimeoutAfter), Url (..), attempted, fetch, tried)
import Effectful.Http             qualified as Http
import Effectful.Http.Duration    (Duration)
import Network.HTTP.Types         (Header, HeaderName, Query, hAuthorization, hUserAgent)

-- * Composing

{- | A middleware stack assembled as a value. @'<>'@ keeps the left one
closer to the program, so 'mconcat' reads outwards from the caller towards
the network.
-}
newtype Middleware es = Middleware (forall a. Eff es a -> Eff es a)

instance Semigroup (Middleware es) where
  Middleware nearer <> Middleware further = Middleware $ further . nearer

instance Monoid (Middleware es) where
  mempty = Middleware id

apply :: Middleware es -> Eff es a -> Eff es a
apply (Middleware f) = f

-- * Building your own

{- | Rewrite every request on its way out. Every middleware in the
"Headers", "Addressing" and "Transport" sections below is this plus a
function.
-}
shaping :: (Http :> es) => (Request -> Request) -> Eff es a -> Eff es a
shaping rewrite = interpose_ $ \(Fetch req) -> fetch (rewrite req)

{- | Watch every attempt without altering it — the hook for logging, metrics
or tracing. Placed nearer the wire than 'Effectful.Http.Retry.withRetry'
it sees each attempt; placed nearer the program it sees only the outcome.

Non-altering includes the error: whatever 'Error' the wrapped 'fetch'
throws (or does not) still reaches the caller unchanged, observed but not
intercepted.
-}
withObserver ::
  (Http :> es, Error Http.Error :> es) => (Request -> Attempted -> Eff es ()) -> Eff es a -> Eff es a
withObserver observe = interpose_ $ \(Fetch req) -> do
  outcome <- tried $ fetch req
  observe req $ attempted outcome
  either throwError pure outcome

-- * Headers

-- | Set these headers, replacing any of the same name the request carried.
withHeaders :: (Http :> es) => [Header] -> Eff es a -> Eff es a
withHeaders extra = shaping $ \req -> req{headers = extra <> without (names extra) req.headers}

-- | Set these headers only where the request did not name them itself.
withDefaultHeaders :: (Http :> es) => [Header] -> Eff es a -> Eff es a
withDefaultHeaders extra =
  shaping $ \req -> req{headers = req.headers <> without (names req.headers) extra}

names :: [Header] -> [HeaderName]
names = map fst

without :: [HeaderName] -> [Header] -> [Header]
without taken = filter $ (`notElem` taken) . fst

withBearerAuth :: (Http :> es) => ByteString -> Eff es a -> Eff es a
withBearerAuth token = withHeaders [(hAuthorization, "Bearer " <> token)]

withBasicAuth :: (Http :> es) => ByteString -> ByteString -> Eff es a -> Eff es a
withBasicAuth user secret =
  withHeaders [(hAuthorization, "Basic " <> Base64.encode (user <> ":" <> secret))]

withApiKey :: (Http :> es) => HeaderName -> ByteString -> Eff es a -> Eff es a
withApiKey name key = withHeaders [(name, key)]

withUserAgent :: (Http :> es) => ByteString -> Eff es a -> Eff es a
withUserAgent agent = withDefaultHeaders [(hUserAgent, agent)]

-- * Addressing

{- | Resolve every request against this base: its scheme, host and port
replace the request's, and its path becomes a prefix.

The 'Url' DSL already composes — @'Effectful.Http.https' "api.example.com"
'Effectful.Http./:' "v1"@ is a perfectly good base to build call sites
from — so reach for this to redirect code you do not control, at a staging
host or a test double.
-}
withBaseUrl :: (Http :> es) => Url -> Eff es a -> Eff es a
withBaseUrl base = shaping $ \req -> req{url = base <> req.url}

-- | Prefix every request's path, leaving its authority alone.
withPathPrefix :: (Http :> es) => [PathSegment] -> Eff es a -> Eff es a
withPathPrefix prefix = shaping $ \req -> req{url = req.url{path = prefix <> req.url.path}}

-- | Add these query parameters to every request, keeping whatever it had.
withQuery :: (Http :> es) => Query -> Eff es a -> Eff es a
withQuery extra = shaping $ \req -> req{url = req.url{query = extra <> req.url.query}}

-- * Transport

-- | Override whatever timeout each request asked for.
withTimeout :: (Http :> es) => Duration -> Eff es a -> Eff es a
withTimeout d = shaping $ \req -> req{timeout = TimeoutAfter d}

-- | Override whatever redirect policy each request asked for.
withRedirects :: (Http :> es) => Redirects -> Eff es a -> Eff es a
withRedirects redirects = shaping $ \req -> req{redirects}
