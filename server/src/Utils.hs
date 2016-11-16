module Utils where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Aeson
import           Data.Functor.Identity
import           Data.Time
import qualified Network.WebSockets     as WS

threadPause :: NominalDiffTime -> IO ()
threadPause = threadDelay . floor . (1000 * 1000 *) . toRational

singleton :: a -> [a]
singleton x = [x]

addTime :: Int -> UTCTime -> UTCTime
addTime d = addUTCTime (fromIntegral d)

toMessage
  :: ToJSON a
  => a -> WS.Message
toMessage = WS.DataMessage . WS.Text . encode

runStateSTM :: TVar a -> StateT a Identity b -> STM (b, a)
runStateSTM var f = do
  value <- readTVar var
  let (v, new) = runIdentity $ runStateT f value
  writeTVar var new
  return (v, new)
