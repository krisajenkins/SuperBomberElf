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
import           Data.UUID
import           Engine
import           Levels
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

displayPosition :: Position -> Value
displayPosition Position{..} = object [("x",toJSON _x),("y",toJSON _y)]

displayPlayer :: Player -> Value
displayPlayer Player{..} =
  object [("position",displayPosition _playerPosition)
         ,("name",toJSON _playerName)
         ,("alive",toJSON (isNothing _playerDiedAt))]

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
         ,("players",toJSON (displayPlayer <$> Map.elems (view players s)))
         ,("walls",toJSON (displayWall <$> view walls s))]



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
                       ,_playerPosition = Position 1 1}
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

playerLoop :: TVar Server -> ClientId  -> IO ()
playerLoop server clientId = forever $ handleCommandFromPlayer server clientId

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
     case eitherDecode rawMsg :: Either String PlayerCommand of
       Left e ->
         do printf "ERR: %s\n" (show e)
            WS.send conn (toMessage invalidMessageHelp)
       Right cmd ->
         do atomically $ processMessage server (FromPlayer clientId cmd)
            sendSceneToPlayers server
            threadDelay (100 * 1000)

processMessage :: TVar Server -> ServerCommand -> STM ()
processMessage server message =
  modifyTVar server
             (over scene (Engine.update message))

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
  do _ <- threadDelay (200 * 1000)
     t <- getCurrentTime
     atomically $
       processMessage server
                      (Tick t)
     sendSceneToPlayers server

run :: IO ()
run = runGameServer defaultConfig

aWall =
  do t <- getCurrentTime
     let _wallPosition = Position 4 5
         _wallType = Strong
         _wallDiedAt = Just $ addUTCTime (fromRational (-10.0)) t
         -- _wallOwner = ClientId  uuid
         -- _blast = Just $ Blast (Map.fromList [(East,3)])
         wall = Wall {..}
     print $ encode (displayWall wall)

aBomb =
  do t <- getCurrentTime
     let _bombPosition = Position 4 5
         _bombExplodesAt = t
         Just uuid = Data.UUID.fromString "4ea7727c-11d3-44d7-9db8-7cfc88b93690"
         _bombOwner = ClientId  uuid
         _blast = Just $ Blast (Map.fromList [(East,3)])
         bomb = Bomb {..}
     print $ encode (displayBomb bomb)
