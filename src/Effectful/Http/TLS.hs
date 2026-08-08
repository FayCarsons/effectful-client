{- | Connection pools that speak TLS, for callers who have no 'Manager' of
their own.

This module exists so that @http-client-tls@ — and with it the whole
@tls@ and @crypton@ stack — is a dependency only where it is wanted. It is
built under the @tls@ package flag, which is on by default; turn the flag off
and supply 'Effectful.Http.runHttp' a 'Manager' built however you like.
-}
module Effectful.Http.TLS (
  newHttpManager,
  defaultHttpManager,
) where

import Effectful
import Network.HTTP.Client     (Manager, ManagerSettings)
import Network.HTTP.Client.TLS (newTlsManager, newTlsManagerWith)

{- | Allocate the connection pool once, at boot, and share the 'Manager'
across every 'Effectful.Http.runHttp'. A manager per request defeats
keep-alive.
-}
newHttpManager :: (IOE :> es) => ManagerSettings -> Eff es Manager
newHttpManager = newTlsManagerWith

-- | 'newHttpManager' with @http-client-tls@'s own settings.
defaultHttpManager :: (IOE :> es) => Eff es Manager
defaultHttpManager = newTlsManager
