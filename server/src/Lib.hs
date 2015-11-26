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
import           Data.Maybe
import qualified Data.Text              as T
import           Data.Time
import           Engine
import           Levels
import qualified Network.WebSockets     as WS
import           System.Random
import           Text.Printf
import           Types
import           Utils

data Server =
  Server {_clients :: Map ClientId WS.Connection
         ,_scene   :: Scene
         ,_gen     :: StdGen}

makeLenses ''Server

toMessage :: ToJSON a => a -> WS.Message
toMessage = WS.DataMessage . WS.Text . encode

------------------------------------------------------------

displayPosition :: Position -> Value
displayPosition Position{..} = object [("x",toJSON _x),("y",toJSON _y)]

displayPlayer :: ClientId -> Player -> Value
displayPlayer playerId Player{..} =
  object [("id",toJSON playerId)
         ,("name",toJSON _playerName)
         ,("position",displayPosition _playerPosition)
         ,("alive",toJSON (isNothing _playerDiedAt))
         ,("score",toJSON _playerScore)]

displayBomb :: Bomb -> Value
displayBomb bomb =
  object [("position",displayPosition (view bombPosition bomb))
         ,("blast", toJSON (view blast bomb))]

displayWall :: Wall -> Value
displayWall Wall{..} =
  object [("position",displayPosition _wallPosition)
         ,("alive",toJSON (isNothing _wallDiedAt))
         ,("type",toJSON _wallType)]

displayScene :: Scene -> Value
displayScene s =
  object [("bombs",toJSON (displayBomb <$> view bombs s))
         ,("walls",toJSON (displayWall <$> view walls s))
         ,("players"
          ,toJSON (Map.foldWithKey (\k v b -> displayPlayer k v : b)
                                   []
                                   (view players s)))]

------------------------------------------------------------

playerJoins :: TVar Server -> WS.Connection -> IO ClientId
playerJoins server conn =
  do (clientId,newServerState) <-
       atomically $
       do serverState <- readTVar server
          let (uuid,_gen') = random (view gen serverState)
              newClientId = ClientId uuid
              newPlayer =
                Player {_playerName = Just . T.pack $ show uuid
                       ,_playerDiedAt = Nothing
                       ,_playerPosition = Position 1 1
                       ,_playerScore = 0}
          modifyTVar
            server
            (over (scene . players)
                  (Map.insert newClientId newPlayer) .
             over clients (Map.insert newClientId conn) . set gen _gen')
          newServerState <- readTVar server
          return (newClientId,newServerState)
     printf "CONNECTED: %s\n" (show clientId)
     sendSceneToConnection (view scene newServerState)
                           conn
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

invalidMessageHelp :: Value
invalidMessageHelp =
  object ["validCommands" .= allPlayerCommands
         , "hint" .=
          String "Messages must be valid JSON, and plain strings aren't allowed as top-level JSON elements, so everything must be wrapped in a message object. Blame Doug Crockford, not me!"]

handleCommandFromPlayer :: TVar Server -> ClientId -> IO ()
handleCommandFromPlayer server clientId =
  do serverState <- atomically $ readTVar server
     let (Just conn) = view (clients . at clientId) serverState
     dataMessage <- WS.receiveDataMessage conn
     handleMessage server clientId dataMessage

handleMessage :: TVar Server -> ClientId -> WS.DataMessage -> IO ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) =
  do serverState <- atomically $ readTVar server
     let (Just conn) = view (clients . at clientId) serverState
     case eitherDecode rawMsg of
       Left e ->
         do printf "ERR %s: %s\n"
                   (show clientId)
                   (show e)
            WS.send conn (toMessage invalidMessageHelp)
       Right cmd ->
         do atomically $
              processMessage server
                             (FromPlayer clientId cmd)
            sendSceneToPlayers server
            threadPause playerThrottleDelay

processMessage :: TVar Server -> ServerCommand -> STM ()
processMessage server message =
  modifyTVar server
             (over scene (Engine.update message))

playerLoop :: TVar Server -> ClientId  -> IO ()
playerLoop server clientId = forever $ handleCommandFromPlayer server clientId

sendSceneToPlayers :: TVar Server -> IO ()
sendSceneToPlayers server =
  do serverState <- atomically $ readTVar server
     mapM_ (sendSceneToConnection (view scene serverState))
           (Map.elems (view clients serverState))

sendSceneToConnection :: Scene -> WS.Connection -> IO ()
sendSceneToConnection s conn = WS.send conn (WS.DataMessage . WS.Text . encode . displayScene $ s)

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
     _ <- forkIO $ gameLoop server
     runWebsocketServer (view playersBindTo config)
                          (acceptPlayerConnection server)
     printf "STOP\n"

gameLoop :: TVar Server -> IO ()
gameLoop server =
  forever $
  do _ <- threadPause frameDelay
     t <- getCurrentTime
     atomically $
       processMessage server
                      (Tick t)
     sendSceneToPlayers server

run :: IO ()
run = runGameServer defaultConfig
