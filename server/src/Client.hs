{-# LANGUAGE OverloadedStrings #-}
module Client where

import           Config
import           Control.Concurrent
import           Control.Lens
import           Data.Time
import qualified Network.WebSockets as WS
import           Utils

websocketHandler :: Int -> WS.ClientApp ()
websocketHandler n conn = do
  _ <- WS.receiveDataMessage conn
  websocketHandler n conn

runWebsocketClient :: Config -> IO ()
runWebsocketClient config = do
  mapM_
    (forkIO .
     WS.runClient
       (view (playersBindTo . address) config)
       (view (playersBindTo . port) config)
       "/" .
     websocketHandler)
    [0 .. 10]
  threadPause (60 :: NominalDiffTime)

run :: IO ()
run = loadConfig >>= either print runWebsocketClient
