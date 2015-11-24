{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Lib (run) where

import           Config
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens           hiding ((.=))
import           Control.Monad
import           Data.Aeson

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Time
import           Engine
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
                       ,_playerPosition = Position 1 1}
          modifyTVar
            server
            (over (scene . players)
                  (Map.insert newClientId newPlayer) .
             over clients (Map.insert newClientId conn) . set gen _gen')
          return newClientId
     printf "CONNECTED: %s\n" (show clientId)
     s <- atomically (view scene <$> readTVar server)
     sendSceneToConnection s conn
     return clientId

playerLeaves :: TVar Server -> ClientId -> IO ()
playerLeaves server clientId =
  do atomically $
       modifyTVar
         server
         (over clients (Map.delete clientId) .
          over (scene . players)
               (Map.delete clientId))
     printf "DISCONNECTED: %s\n" (show clientId)

allMessages :: [PlayerMessage]
allMessages = PlayerMessage <$> [minBound ..]

invalidMessageHelp :: Value
invalidMessageHelp =
  object ["validMessages" .= allMessages
         ,"hint" .=
          String "Messages must be valid JSON, and plain strings aren't allowed as top-level JSON elements, so everything must be wrapped in a message object. Blame Doug Crockford, not me!"]

playerLoop :: TVar Server -> ClientId  -> IO ()
playerLoop server clientId =
  forever $ handleCommandFromPlayer server clientId

handleCommandFromPlayer :: TVar Server -> ClientId -> IO ()
handleCommandFromPlayer server clientId =
  do serverState <- atomically $ readTVar server
     let (Just conn) = view (clients . at clientId) serverState
     dataMessage <- WS.receiveDataMessage conn
     printf "Got RawMsg: '%s'\n" (show dataMessage)
     handleMessage server clientId dataMessage

handleMessage :: TVar Server -> ClientId -> WS.DataMessage -> IO ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) =
  do serverState <- atomically $ readTVar server
     let (Just conn) = view (clients . at clientId) serverState
     case eitherDecode rawMsg :: Either String PlayerMessage of
       Left e ->
         do printf "ERR: %s\n" (show e)
            WS.send conn (toMessage invalidMessageHelp)
       Right (PlayerMessage msg) ->
         do atomically $
              modifyTVar server
                         (over scene (Engine.update (FromPlayer clientId msg)))
            sendSceneToPlayers server
            threadDelay (100 * 1000)

sendSceneToPlayers :: TVar Server -> IO ()
sendSceneToPlayers server =
  do serverState <- atomically $ readTVar server
     mapM_ (sendSceneToConnection (view scene serverState))
           (Map.elems (view clients serverState))

sendSceneToConnection :: Scene -> WS.Connection -> IO ()
sendSceneToConnection s conn = WS.send conn (toMessage s)

acceptPlayerConnection :: TVar Server -> WS.ServerApp
acceptPlayerConnection server pendingConnection =
  do conn <- WS.acceptRequest pendingConnection
     clientId <- playerJoins server conn
     finally (playerLoop server clientId)
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
