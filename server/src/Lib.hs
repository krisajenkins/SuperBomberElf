{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Lib (run,genUUID,createPlayer,Server(..),generator) where

import           Config
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens                   hiding ((.=))
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Data.Time
import           Data.UUID
import           Engine
import           Levels
import qualified Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import qualified Network.WebSockets             as WS
import           Render
import qualified Rest
import           System.Random
import           Text.Printf
import           Types
import           Utils

data Server =
  Server {_clients   :: Map ClientId WS.Connection
         ,_scene     :: Scene
         ,_generator :: StdGen}
makeLenses ''Server

------------------------------------------------------------

readRandom :: (Random a,Monad m) => StateT Server m a
readRandom =
  do (r,g') <- random <$> use generator
     assign generator g'
     return r

readRandomR :: (Random a,Monad m) => (a,a) -> StateT Server m a
readRandomR range =
  do (r,g') <- randomR range <$> use generator
     assign generator g'
     return r

genUUID :: State Server UUID
genUUID = readRandom

------------------------------------------------------------

genStartPosition :: State Server Position
genStartPosition =
  do n <- readRandomR (0,length validStartPositions - 1)
     return $ validStartPositions !! n

createPlayer :: State Server (ClientId,Player)
createPlayer =
  do uuid <- genUUID
     startPosition <- genStartPosition
     return $
       (,) (ClientId uuid)
           Player {_playerName = T.pack $ show uuid
                  ,_playerDiedAt = Nothing
                  ,_playerPosition = startPosition
                  ,_playerScore = 0}

handleCommandFromPlayer :: TVar Server -> ClientId -> IO ()
handleCommandFromPlayer server clientId =
  do serverState <- atomically $ readTVar server
     (Just conn) <- pure $ view (clients . at clientId) serverState
     dataMessage <- WS.receiveDataMessage conn
     handleMessage server clientId dataMessage

handleMessage :: TVar Server -> ClientId -> WS.DataMessage -> IO ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) =
  do serverState <- atomically $ readTVar server
     mConnection <- pure $ view (clients . at clientId) serverState
     case mConnection of
       Nothing ->
         printf "SERVER ERROR - CONNECTION NOT FOUND: %s\n" (show clientId)
       Just conn ->
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

------------------------------------------------------------

acceptPlayerConnection :: TVar Server -> WS.ServerApp
acceptPlayerConnection server pendingConnection =
  bracket (WS.acceptRequest pendingConnection >>= playerJoins server)
          (playerLeaves server)
          (playerLoop server)

addConnection :: Connection -> State Server ClientId
addConnection conn =
  do (newClientId,newPlayer) <- createPlayer
     assign (scene . players . at newClientId) (Just newPlayer)
     assign (clients . at newClientId) (Just conn)
     return newClientId

removeConnection :: ClientId -> State Server ()
removeConnection clientId =
  do assign (scene . players . at clientId) Nothing
     assign (clients . at clientId) Nothing

playerJoins :: TVar Server -> WS.Connection -> IO ClientId
playerJoins server conn =
  do (clientId,newServerState) <-
       atomically . runStateSTM server $ addConnection conn
     printf "JOINED: %s\n" (show clientId)
     sendSceneToConnection (view scene newServerState)
                           conn
     return clientId

playerLeaves :: TVar Server -> ClientId -> IO ()
playerLeaves server clientId =
  do atomically . modifyTVar server . execState $ removeConnection clientId
     printf "DISCONNECTED: %s\n" (show clientId)

playerLoop :: TVar Server -> ClientId  -> IO ()
playerLoop server clientId = forever $ handleCommandFromPlayer server clientId

invalidMessageHelp :: Value
invalidMessageHelp =
  object ["validCommands" .= allPlayerCommands "<name>"
         ,"hint" .=
          String "Messages must be valid JSON, and plain strings aren't allowed as top-level JSON elements, so everything must be wrapped in a message object. Blame Doug Crockford, not me!"]

sendSceneToPlayers :: TVar Server -> IO ()
sendSceneToPlayers server =
  do serverState <- atomically $ readTVar server
     mapM_ (sendSceneToConnection (view scene serverState))
           (Map.elems (view clients serverState))

sendSceneToConnection :: Scene -> WS.Connection -> IO ()
sendSceneToConnection =
  flip WS.send . WS.DataMessage . WS.Text . encode . displayScene

------------------------------------------------------------

processGameEvent :: TVar Server -> GameEvent -> STM ()
processGameEvent server event =
  modifyTVar server
             (over scene (Engine.handleGameEvent event))

------------------------------------------------------------

gameLoop :: TVar Server -> IO ()
gameLoop server =
  forever $
  do tick <- Tick <$> getCurrentTime
     atomically $ processGameEvent server tick
     sendSceneToPlayers server
     threadPause frameDelay

------------------------------------------------------------

initServer :: IO (TVar Server)
initServer =
  do _generator <- getStdGen
     _scene <- initialScene <$> getCurrentTime
     _clients <- pure Map.empty
     atomically $ newTVar Server {..}

runGameServer :: Config -> IO ()
runGameServer config =
  do printf "Starting game loop thread.\n"
     server <- initServer
     _ <- forkIO $ gameLoop server
     let p = view (playersBindTo . port) config
     printf "Starting webserver on: %d\n" p
     Warp.run p
              (websocketsOr defaultConnectionOptions
                            (acceptPlayerConnection server)
                            (Rest.application (view staticDir config)))
     printf "Finished.\n"

run :: IO ()
run = loadConfig >>= either print runGameServer
