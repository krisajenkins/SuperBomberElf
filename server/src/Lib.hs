{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( run
  , Server(..)
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
  { _clients :: Map ClientId ClientConnection
  , _scene   :: Scene
  }

makeLenses ''Server

------------------------------------------------------------
class Monad m =>
      MonadSTM m  where
  atomicallyM :: STM a -> m a

instance MonadSTM IO where
  atomicallyM = atomically

instance MonadSTM m =>
         MonadSTM (ResourceT m) where
  atomicallyM = lift . atomicallyM

instance MonadSTM m =>
         MonadSTM (ReaderT env m) where
  atomicallyM = lift . atomicallyM

instance MonadSTM m =>
         MonadSTM (LoggingT m) where
  atomicallyM = lift . atomicallyM

instance MonadSTM m =>
         MonadSTM (RandT StdGen m) where
  atomicallyM = lift . atomicallyM

------------------------------------------------------------
class Monad m =>
      MonadTime m  where
  now :: m UTCTime
  pause :: NominalDiffTime -> m ()

instance MonadTime IO where
  now = getCurrentTime
  pause = threadPause

instance MonadTime m =>
         MonadTime (ResourceT m) where
  now = lift now
  pause = lift . pause

instance MonadTime m =>
         MonadTime (ReaderT a m) where
  now = lift now
  pause = lift . pause

instance MonadTime m =>
         MonadTime (LoggingT m) where
  now = lift now
  pause = lift . pause

------------------------------------------------------------
class Monad m =>
      MonadWebsocket m  where
  send :: ClientConnection -> WS.Message -> m ()
  receive :: ClientConnection -> m WS.DataMessage

instance MonadWebsocket IO where
  send client msg = liftIO $ WS.send (view connection client) msg
  receive = liftIO . WS.receiveDataMessage . view connection

instance MonadWebsocket m =>
         MonadWebsocket (ResourceT m) where
  send client = lift . send client
  receive = lift . receive

instance MonadWebsocket m =>
         MonadWebsocket (RandT StdGen m) where
  send client = lift . send client
  receive = lift . receive

instance MonadWebsocket m =>
         MonadWebsocket (LoggingT m) where
  send client = lift . send client
  receive = lift . receive

sendJson
  :: (MonadWebsocket m, ToJSON a)
  => ClientConnection -> a -> m ()
sendJson client value = send client (WS.DataMessage . WS.Text . encode $ value)

------------------------------------------------------------
genStartPosition
  :: MonadRandom m
  => m Position
genStartPosition = do
  n <- getRandomR (0, length validStartPositions - 1)
  pure $ validStartPositions !! n

createPlayer
  :: MonadRandom m
  => m (ClientId, Player)
createPlayer = do
  uuid <- getRandom
  startPosition <- genStartPosition
  pure
    ( ClientId uuid
    , Player
      { _playerName = T.pack $ show uuid
      , _playerDiedAt = Nothing
      , _playerPosition = startPosition
      , _playerScore = 0
      })

handleCommandFromClient
  :: (MonadLogger m, MonadTime m, MonadWebsocket m, MonadSTM m)
  => TVar Server -> ClientId -> m ()
handleCommandFromClient server clientId = do
  serverState <- atomicallyM $ readTVar server
  let Just clientConnection = view (clients . at clientId) serverState
  dataMessage <- receive clientConnection
  handleMessage server clientId dataMessage

handleMessage
  :: (MonadLogger m, MonadTime m, MonadWebsocket m, MonadSTM m)
  => TVar Server -> ClientId -> WS.DataMessage -> m ()
handleMessage _ _ (WS.Text "") = pure ()
handleMessage _ _ (WS.Binary _) = pure ()
handleMessage server clientId (WS.Text rawMsg) = do
  serverState <- atomicallyM $ readTVar server
  case view (clients . at clientId) serverState of
    Nothing -> logErrorN $ F.sformat ("CONNECTION NOT FOUND: " % F.shown) clientId
    Just clientConnection ->
      case eitherDecode rawMsg of
        Left e -> do
          logErrorN $ F.sformat (F.shown % " - " % F.shown) clientId e
          sendJson clientConnection helpMessage
        Right cmd -> do
          newServerState <-
            atomicallyM $
            do processGameEvent server (FromPlayer clientId cmd)
               readTVar server
          sendJson clientConnection (views scene displayScene newServerState)
          pause playerThrottleDelay

------------------------------------------------------------
joinPlayer
  :: Monad m
  => WS.Connection -> ClientId -> Player -> StateT Server m ()
joinPlayer conn newClientId newPlayer = do
  assign (scene . players . at newClientId) (Just newPlayer)
  assign (clients . at newClientId) (Just (ClientConnection conn))

removeConnection
  :: Monad m
  => ClientId -> StateT Server m ()
removeConnection clientId = do
  assign (scene . players . at clientId) Nothing
  assign (clients . at clientId) Nothing

clientJoins
  :: (MonadSTM m, MonadLogger m, MonadWebsocket m, MonadRandom m)
  => TVar Server -> WS.Connection -> m ClientId
clientJoins serverVar conn = do
  (clientId, newPlayer) <- createPlayer
  (_, newServerState) <-
    atomicallyM . stateTVar serverVar $ joinPlayer conn clientId newPlayer
  logInfoN $ F.sformat ("JOINED: " % F.shown) clientId
  let Just clientConnection = view (clients . at clientId) newServerState
  sendJson clientConnection helpMessage
  sendJson clientConnection (views scene displayScene newServerState)
  pure clientId

stateTVar :: TVar s -> StateT s STM a -> STM (a, s)
stateTVar var st = do
  value <- readTVar var
  (result, newValue) <- runStateT st value
  writeTVar var newValue
  return (result, newValue)

clientLeaves
  :: (MonadSTM m, MonadLogger m)
  => TVar Server -> ClientId -> m ()
clientLeaves server clientId = do
  atomicallyM . modifyTVar' server . execState $ removeConnection clientId
  logInfoN $ F.sformat ("DISCONNECTED: " % F.shown) clientId

clientLoop
  :: (MonadSTM m, MonadLogger m, MonadTime m, MonadWebsocket m)
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
runGameClock
  :: (MonadSTM m, MonadTime m)
  => TVar Server -> m ()
runGameClock server = do
  tick <- Tick <$> now
  _ <- atomicallyM $ processGameEvent server tick
  pause frameDelay

------------------------------------------------------------
instance MonadRandom m =>
         MonadRandom (LoggingT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

instance MonadLogger m => MonadLogger (RandT StdGen m)

acceptClientConnection
  :: (MonadResource m
     ,MonadLogger m
     ,MonadTime m
     ,MonadWebsocket m
     ,MonadSTM m
     ,MonadIO m)
  => TVar Server -> WS.PendingConnection -> m ()
acceptClientConnection server pendingConnection = do
  (_releaseKey, resource) <-
    allocate
      (runStdoutLoggingT $
       do gen <- liftIO getStdGen
          conn <- liftIO $ WS.acceptRequest pendingConnection
          fst <$> runRandT (clientJoins server conn) gen -- TODO
       )
      (runStdoutLoggingT . clientLeaves server)
  clientLoop server resource

------------------------------------------------------------
initServer
  :: (MonadTime m, MonadSTM m)
  => m (TVar Server)
initServer = do
  _scene <- initialScene <$> now
  _clients <- pure Map.empty
  atomicallyM $
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
  :: (MonadIO m, MonadLogger m,  MonadTime m, MonadSTM m)
  => Config -> m ()
runGameServer config = do
  logInfoN "Starting EKG"
  _ <- startEkg ekgPort
  server <- initServer
  logInfoN "Starting game clock thread."
  _ <- liftIO . forkIO . forever . runGameClock $ server
  logInfoN $ F.sformat ("Starting webserver on: " % F.int) websocketPort
  liftIO . Warp.run websocketPort $
    websocketsOr
      WS.defaultConnectionOptions
      (runStdoutLoggingT . runResourceT . acceptClientConnection server)
      (Rest.application static)
  logInfoN "Finished."
  where
    ekgPort = view ekgBindsTo config
    websocketPort = view (playersBindTo . port) config
    static = view staticDir config

run :: IO ()
run = loadConfig >>= either print (runStdoutLoggingT  . runGameServer)
