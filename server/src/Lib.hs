{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( run
  , genUUID
  , createPlayer
  , Server(..)
  , generator
  ) where

import           Config
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens                   hiding ((.=))
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Data.Time
import           Data.UUID
import           Engine
import           Formatting                     ((%))
import qualified Formatting                     as F
import           Levels
import qualified Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets             as WS
import           Render
import qualified Rest
import           System.Random                  (Random, StdGen, getStdGen,
                                                 random, randomR)
import qualified System.Remote.Monitoring       as EKG
import           Types
import           Utils

data ClientConnection = ClientConnection
  { _connection :: WS.Connection
  }

makeLenses ''ClientConnection

data Server = Server
  { _clients   :: Map ClientId ClientConnection
  , _scene     :: Scene
  , _generator :: StdGen
  }

makeLenses ''Server

------------------------------------------------------------

genUUID
  :: Monad m
  => StateT StdGen m UUID
genUUID = state random

genRange
  :: (Random a, Monad m)
  => (a, a) -> StateT StdGen m a
genRange = state . randomR

------------------------------------------------------------

genStartPosition :: Monad m => StateT StdGen m Position
genStartPosition = do
  n <- genRange (0, length validStartPositions - 1)
  return $ validStartPositions !! n

createPlayer :: Monad m => StateT StdGen m (ClientId, Player)
createPlayer = do
  uuid <- genUUID
  startPosition <- genStartPosition
  return
    ( ClientId uuid
    , Player
      { _playerName = T.pack $ show uuid
      , _playerDiedAt = Nothing
      , _playerPosition = startPosition
      , _playerScore = 0
      })

handleCommandFromClient :: TVar Server -> ClientId -> IO ()
handleCommandFromClient server clientId = do
  serverState <- atomically $ readTVar server
  (Just clientConnection) <- pure $ view (clients . at clientId) serverState
  dataMessage <- WS.receiveDataMessage (view connection clientConnection)
  handleMessage server clientId dataMessage

handleMessage :: TVar Server -> ClientId -> WS.DataMessage -> IO ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) = do
  serverState <- atomically $ readTVar server
  mClientConnection <- pure $ view (clients . at clientId) serverState
  case mClientConnection of
    Nothing ->
      F.fprint ("SERVER ERROR - CONNECTION NOT FOUND: " % F.shown % "\n") clientId
    Just clientConnection ->
      case eitherDecode rawMsg of
        Left e -> do
          F.fprint ("ERR " % F.shown % ": " % F.shown % "\n") clientId e
          WS.send (view connection clientConnection) (toMessage helpMessage)
        Right cmd -> do
          newServerState <-
            atomically $
            do processGameEvent server (FromPlayer clientId cmd)
               readTVar server
          sendSceneToClient (view scene newServerState) clientConnection
          threadPause playerThrottleDelay

------------------------------------------------------------
acceptClientConnection :: TVar Server -> WS.ServerApp
acceptClientConnection server pendingConnection =
  bracket
    (WS.acceptRequest pendingConnection >>= clientJoins server)
    (clientLeaves server)
    (clientLoop server)

addConnection :: WS.Connection -> State Server ClientId
addConnection conn = do
  (newClientId, newPlayer) <- zoom generator createPlayer
  assign (scene . players . at newClientId) (Just newPlayer)
  assign (clients . at newClientId) (Just (ClientConnection conn))
  return newClientId

removeConnection :: ClientId -> State Server ()
removeConnection clientId = do
  assign (scene . players . at clientId) Nothing
  assign (clients . at clientId) Nothing

clientJoins :: TVar Server -> WS.Connection -> IO ClientId
clientJoins server conn = do
  (clientId, newServerState) <-
    atomically . runStateSTM server $ addConnection conn
  F.fprint ("JOINED: " % F.shown % "\n") clientId
  Just clientConnection <- pure $ view (clients . at clientId) newServerState
  WS.send (view connection clientConnection) (toMessage helpMessage)
  sendSceneToClient (view scene newServerState) clientConnection
  return clientId

clientLeaves :: TVar Server -> ClientId -> IO ()
clientLeaves server clientId = do
  atomically . modifyTVar server . execState $ removeConnection clientId
  F.fprint ("DISCONNECTED: " % F.shown % "\n") clientId

clientLoop :: TVar Server -> ClientId -> IO ()
clientLoop server clientId = forever $ handleCommandFromClient server clientId

helpMessage :: Value
helpMessage =
  object
    [ "validCommands" .= allPlayerCommands "<name>"
    , "hint" .=
      String
        "Messages must be valid JSON, and plain strings aren't allowed as top-level JSON elements, so everything must be wrapped in a message object. Blame Doug Crockford, not me!"
    ]

sendSceneToClient :: Scene -> ClientConnection -> IO ()
sendSceneToClient currentScene clientConnection =
  WS.send
    (view connection clientConnection)
    (WS.DataMessage . WS.Text . encode . displayScene $ currentScene)

------------------------------------------------------------
processGameEvent :: TVar Server -> GameEvent -> STM ()
processGameEvent server event =
  modifyTVar server (over scene (Engine.handleGameEvent event))

------------------------------------------------------------
-- TODO The frame rate is actually frameDelay *plus* the time it takes
--  to get the tick processed. Not ideal.
gameLoop :: TVar Server -> IO ()
gameLoop server =
  forever $
  do tick <- Tick <$> getCurrentTime
     _ <- atomically $ processGameEvent server tick
     threadPause frameDelay

------------------------------------------------------------
initServer :: IO (TVar Server)
initServer = do
  _generator <- getStdGen
  _scene <- initialScene <$> getCurrentTime
  _clients <- pure Map.empty
  atomically $
    newTVar
      Server
      { ..
      }

startEkg :: BindTo -> IO EKG.Server
startEkg bindTo = do
  F.fprint ("Monitoring on port: " % F.shown % "\n") bindTo
  EKG.forkServer (BS.pack (view address bindTo)) (view port bindTo)

runGameServer :: Config -> IO ()
runGameServer config = do
  _ <- startEkg (view ekgBindsTo config)
  server <- initServer
  F.fprint "Starting game loop thread.\n"
  _ <- forkIO $ gameLoop server
  let p = view (playersBindTo . port) config
  _ <- F.fprint ("Starting webserver on: " % F.int % "\n") p
  Warp.run
    p
    (websocketsOr
       WS.defaultConnectionOptions
       (acceptClientConnection server)
       (Rest.application (view staticDir config)))
  F.fprint "Finished.\n"

run :: IO ()
run = loadConfig >>= either print runGameServer
