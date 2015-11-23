module Lib (server) where

import           Config
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Engine
import qualified Network.WebSockets as WS
import           Types

toMessage :: ToJSON a => a -> WS.Message
toMessage = WS.DataMessage . WS.Text . encode

------------------------------------------------------------

------------------------------------------------------------

renderToWebsocket :: RenderChan -> WS.Connection -> IO ()
renderToWebsocket renderChan conn =
  forever $
  do scene <- readScene renderChan
     WS.send conn $ toMessage scene
     threadDelay $ 500 * 1000

runWebsocket :: RenderChan -> WS.ServerApp
runWebsocket renderChan pendingConnection =
  do conn <- WS.acceptRequest pendingConnection
     forever $ renderToWebsocket renderChan conn

runWebsocketServer
  :: Config -> RenderChan -> IO ()
runWebsocketServer config chan =
  WS.runServer (view address config)
               (view port config) $
  runWebsocket chan

runGameServer :: Config -> IO ()
runGameServer config = do runWebsocketServer config

k :: IO ()
k = runGameServer defaultConfig
