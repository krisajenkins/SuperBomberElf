{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib (run) where

import           Config
import           Control.Concurrent
-- import           Control.Concurrent.Async
import           Control.Concurrent.STM
-- import           Control.Concurrent.STM.TChan (TChan, readTChan)
import           Control.Exception
import           Control.Lens
import           Control.Monad
-- import           Control.Monad.IO.Class
-- import           Control.Monad.STM            (STM)
import           Data.Aeson
import           Data.Map               (Map)
import qualified Data.Map               as Map
-- import           Data.Text              as T
import           Data.Time
-- import           Data.UUID
-- import           Engine
import qualified Network.WebSockets     as WS
import           System.Random
import           Text.Printf
import           Types

data Server =
  Server {_clients :: Map ClientId WS.Connection
         ,_scene   :: Scene
         ,_gen     :: StdGen}

makeLenses ''Server

toMessage :: ToJSON a => a -> WS.Message
toMessage = WS.DataMessage . WS.Text . encode

------------------------------------------------------------

playerJoins :: TVar Server -> WS.Connection -> IO ClientId
playerJoins server conn =
  do clientId <-
       atomically $
       do serverState <- readTVar server
          let (uuid,_gen') = random (view gen serverState)
              newClientId = ClientId uuid
              newPlayer =
                Player {_playerName = Nothing
                       ,_playerPosition = Position 1 5}
          modifyTVar
            server
            ((over (scene . players)
                   (Map.insert newClientId newPlayer)) .
             (over clients (Map.insert newClientId conn)) . (set gen _gen'))
          return newClientId
     printf "CONNECTED: %s\n" (show clientId)
     return clientId

playerLeaves :: TVar Server -> ClientId -> IO ()
playerLeaves server clientId =
  do atomically $
       modifyTVar
         server
         ((over clients (Map.delete clientId)) .
          (over (scene . players)
                (Map.delete clientId)))
     printf "DISCONNECTED: %s\n" (show clientId)

playerLoop :: TVar Server -> WS.Connection -> IO ()
playerLoop server conn =
  forever $
  do serverState <- atomically $ readTVar server
     WS.send conn (toMessage (view scene serverState))
     --msg <- WS.receiveDataMessage conn
     threadDelay (500 * 1000)

acceptPlayerConnection :: TVar Server -> WS.ServerApp
acceptPlayerConnection server pendingConnection =
  do conn <- WS.acceptRequest pendingConnection
     clientId <- playerJoins server conn
     finally (playerLoop server conn)
             (playerLeaves server clientId)

runWebsocketServer :: BindTo -> WS.ServerApp -> IO ()
runWebsocketServer bindTo =
  WS.runServer (view address bindTo)
               (view port bindTo)

runGameServer :: Config -> IO ()
runGameServer config =
  do printf "START\n"
     _gen <- getStdGen
     _scene <- initialScene <$> getCurrentTime
     let _clients = Map.empty
     server <- atomically $ newTVar Server {..}
     runWebsocketServer (view playersBindTo config)
                        (acceptPlayerConnection server)
     printf "STOP\n"

run :: IO ()
run = runGameServer defaultConfig
