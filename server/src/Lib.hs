{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
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
import           Control.Lens                   hiding ((.=))
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Random           hiding (genRange)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
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
class Monad m =>
      MonadSTM m  where
  atomically2 :: STM a -> m a

instance MonadSTM IO where
  atomically2 = atomically

------------------------------------------------------------
receive
  :: MonadIO m
  => ClientConnection -> m WS.DataMessage
receive = liftIO . WS.receiveDataMessage . view connection

send
  :: (ToJSON a, MonadIO m)
  => ClientConnection -> a -> m ()
send clientConnection value =
  liftIO $ WS.send (view connection clientConnection) (toMessage value)

------------------------------------------------------------
genUUID
  :: Monad m
  => StateT StdGen m UUID
genUUID = state random

------------------------------------------------------------
genStartPosition
  :: MonadRandom m
  => m Position
genStartPosition = do
  n <- getRandomR (0, length validStartPositions - 1)
  return $ validStartPositions !! n

createPlayer
  :: MonadRandom m
  => m (ClientId, Player)
createPlayer = do
  uuid <- getRandom
  startPosition <- genStartPosition
  return
    ( ClientId uuid
    , Player
      { _playerName = T.pack $ show uuid
      , _playerDiedAt = Nothing
      , _playerPosition = startPosition
      , _playerScore = 0
      })

handleCommandFromClient
  :: (MonadIO m, MonadLogger m)
  => TVar Server -> ClientId -> m ()
handleCommandFromClient server clientId = do
  serverState <- liftIO . atomically $ readTVar server
  (Just clientConnection) <- pure $ view (clients . at clientId) serverState
  dataMessage <- receive clientConnection
  handleMessage server clientId dataMessage

handleMessage
  :: (MonadIO m, MonadLogger m)
  => TVar Server -> ClientId -> WS.DataMessage -> m ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) = do
  serverState <- liftIO . atomically $ readTVar server
  mClientConnection <- pure $ view (clients . at clientId) serverState
  case mClientConnection of
    Nothing -> logErrorN $ F.sformat ("CONNECTION NOT FOUND: " % F.shown) clientId
    Just clientConnection ->
      case eitherDecode rawMsg of
        Left e -> do
          logErrorN $ F.sformat (F.shown % " - " % F.shown) clientId e
          send clientConnection helpMessage
        Right cmd -> do
          newServerState <-
            liftIO . atomically $
            do processGameEvent server (FromPlayer clientId cmd)
               readTVar server
          send clientConnection (views scene displayScene newServerState)
          liftIO $ threadPause playerThrottleDelay

------------------------------------------------------------
addConnection
  :: Monad m
  => WS.Connection -> StateT Server m ClientId
addConnection conn = do
  (newClientId, newPlayer) <- zoom generator (state (runRand createPlayer))
  assign (scene . players . at newClientId) (Just newPlayer)
  assign (clients . at newClientId) (Just (ClientConnection conn))
  return newClientId

removeConnection
  :: Monad m
  => ClientId -> StateT Server m ()
removeConnection clientId = do
  assign (scene . players . at clientId) Nothing
  assign (clients . at clientId) Nothing

clientJoins
  :: (MonadIO m, MonadLogger m)
  => TVar Server -> WS.Connection -> m ClientId
clientJoins server conn = do
  (clientId, newServerState) <-
    liftIO . atomically . runStateSTM server $ addConnection conn
  logInfoN $ F.sformat ("JOINED: " % F.shown) clientId
  Just clientConnection <- pure $ view (clients . at clientId) newServerState
  send clientConnection helpMessage
  send clientConnection (views scene displayScene newServerState)
  return clientId

clientLeaves ::
   (MonadIO m, MonadLogger m)
  => TVar Server -> ClientId -> m ()
clientLeaves server clientId = do
  liftIO . atomically . modifyTVar server . execState $ removeConnection clientId
  logInfoN $ F.sformat ("DISCONNECTED: " % F.shown) clientId

clientLoop
  :: (MonadIO m, MonadLogger m)
  => TVar Server -> ClientId -> m ()
clientLoop server = forever . handleCommandFromClient server

helpMessage :: Value
helpMessage =
  object
    [ "validCommands" .= allPlayerCommands "<name>"
    , "hint" .=
      String
        "Messages must be valid JSON, and plain strings aren't allowed as top-level JSON elements, so everything must be wrapped in a message object. Blame Doug Crockford, not me!"
    ]

------------------------------------------------------------
processGameEvent :: TVar Server -> GameEvent -> STM ()
processGameEvent server event =
  modifyTVar server (over scene (Engine.handleGameEvent event))

------------------------------------------------------------
-- TODO The frame rate is actually frameDelay *plus* the time it takes
--  to get the tick processed. Not ideal.
gameStep
  :: MonadIO m
  => TVar Server -> m ()
gameStep server = do
  tick <- liftIO $ Tick <$> getCurrentTime
  _ <- liftIO . atomically $ processGameEvent server tick
  liftIO $ threadPause frameDelay

------------------------------------------------------------
acceptClientConnection
  :: (MonadResource m, MonadLogger m)
  => TVar Server -> WS.PendingConnection -> m ()
acceptClientConnection server pendingConnection = do
  (_releaseKey, resource) <-
    allocate
      (WS.acceptRequest pendingConnection >>= runStdoutLoggingT . clientJoins server)
      (runStdoutLoggingT . clientLeaves server)
  clientLoop server resource

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

startEkg
  :: (MonadIO m, MonadLogger m)
  => BindTo -> m EKG.Server
startEkg bindTo = do
  logInfoN $ F.sformat ("Monitoring on port: " % F.shown) bindTo
  liftIO $ EKG.forkServer (BS.pack (view address bindTo)) (view port bindTo)

runGameServer
  :: (MonadIO m, MonadLogger m, MonadReader Config m)
  => m ()
runGameServer = do
  ekgPort <- view ekgBindsTo
  websocketPort <- view (playersBindTo . port)
  static <- view staticDir
  logInfoN "Starting EKG"
  _ <- startEkg ekgPort
  server <- liftIO initServer
  logInfoN "Starting game loop thread."
  _ <- liftIO . forkIO . forever $ gameStep server
  logInfoN $ F.sformat ("Starting webserver on: " % F.int) websocketPort
  liftIO $
    Warp.run
      websocketPort
      (websocketsOr
         WS.defaultConnectionOptions
         (runStdoutLoggingT . runResourceT . acceptClientConnection server)
         (Rest.application static))
  logInfoN "Finished."

run :: IO ()
run = loadConfig >>= either print (runStdoutLoggingT . runReaderT runGameServer)
