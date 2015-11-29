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
  Server {_clients   :: Map ClientId WS.Connection
         ,_scene     :: Scene
         ,_generator :: StdGen}

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
          let (uuid,g') = random (view generator serverState)
              validStartPositions = do a <- [1,9]
                                       b <- [1,9]
                                       return (Position a b)
              (n, g'') = randomR (0, length validStartPositions - 1) g'
              startPosition = validStartPositions !! n
              newClientId = ClientId uuid
              newPlayer =
                Player {_playerName = Just . T.pack $ show uuid
                       ,_playerDiedAt = Nothing
                       ,_playerPosition = startPosition
                       ,_playerScore = 0}
          modifyTVar
            server
            (set generator g'' .
             over (scene . players)
                  (Map.insert newClientId newPlayer) .
             over clients (Map.insert newClientId conn))
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
              processGameEvent server
                             (FromPlayer clientId cmd)
            sendSceneToPlayers server
            threadPause playerThrottleDelay

processGameEvent :: TVar Server -> GameEvent -> STM ()
processGameEvent server event =
  modifyTVar server
             (over scene (Engine.handleGameEvent event))

playerLoop :: TVar Server -> ClientId  -> IO ()
playerLoop server clientId = forever $ handleCommandFromPlayer server clientId

sendSceneToPlayers :: TVar Server -> IO ()
sendSceneToPlayers server =
  do serverState <- atomically $ readTVar server
     mapM_ (sendSceneToConnection (view scene serverState))
           (Map.elems (view clients serverState))

sendSceneToConnection :: Scene -> WS.Connection -> IO ()
sendSceneToConnection =
  flip WS.send . WS.DataMessage . WS.Text . encode . displayScene

acceptPlayerConnection :: TVar Server -> WS.ServerApp
acceptPlayerConnection server pendingConnection =
  bracket (WS.acceptRequest pendingConnection >>= playerJoins server)
          (playerLeaves server)
          (playerLoop server)

runWebsocketServer :: BindTo -> WS.ServerApp -> IO ()
runWebsocketServer bindTo =
  WS.runServer (view address bindTo)
               (view port bindTo)

runGameServer :: Config -> IO ()
runGameServer config =
  do printf "START\n"
     _generator <- getStdGen
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
       processGameEvent server
                        (Tick t)
     sendSceneToPlayers server

run :: IO ()
run = runGameServer defaultConfig
