{-# LANGUAGE NoOverloadedLists #-}
{- | A real socket that sends the response headers, then resets the
connection in the middle of the body. http-client applies the manager's
exception wrapper only while it opens the response, so without
'runHttp' the reset would escape as a bare 'IOException'.
-}
module ResetSpec (spec) where

import Control.Concurrent        (forkIO, killThread, threadDelay)
import Control.Exception         (bracket)
import Control.Monad             (forever, void)
import Data.ByteString.Lazy      qualified as Lazy
import Data.Word                 (Word16)
import Effectful
import Effectful.Error.Static    (runErrorNoCallStack)
import Effectful.Http
import Network.HTTP.Client       (defaultManagerSettings, newManager)
import Network.HTTP.Types        (methodGet)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Test.Hspec

spec :: Spec
spec =
  describe "a connection reset in the middle of the body" $
    it "fails as Unreachable through Error, not as an exception" $
      withResettingServer $ \port -> do
        manager <- newManager defaultManagerSettings
        outcome <-
          runEff . runErrorNoCallStack @Error . runHttp manager . fetch @Lazy.ByteString $
            request methodGet (withPort port $ http "127.0.0.1" /: "flags")
        outcome `shouldSatisfy` \case
          Left (Unreachable _) -> True
          _                    -> False

{- | Answer every connection with the headers and the first ten bytes of a
thousand-byte body. Wait long enough for the client to start on the body,
then close with a zero linger time, which sends a reset instead of a
graceful close.
-}
withResettingServer :: (Word16 -> IO a) -> IO a
withResettingServer use =
  bracket listening close $ \listener -> do
    port <- socketPort listener
    bracket (forkIO . forever $ serve listener) killThread . const . use $ fromIntegral port
 where
  listening = do
    listener <- socket AF_INET Stream defaultProtocol
    setSocketOption listener ReuseAddr 1
    bind listener . SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
    listen listener 16
    pure listener
  serve listener = do
    (connection, _) <- accept listener
    void $ recv connection 65536
    sendAll connection "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\n0123456789"
    threadDelay 200_000
    setSockOpt connection Linger $ StructLinger 1 0
    close connection
