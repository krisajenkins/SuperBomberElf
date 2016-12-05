module Utils
  ( threadPause
  , runStateSTM
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Time

threadPause :: NominalDiffTime -> IO ()
threadPause = threadDelay . floor . (1000 * 1000 *) . toRational

runStateSTM :: TVar a -> StateT a Identity b -> STM (b, a)
runStateSTM var f = do
  value <- readTVar var
  let (v, new) = runIdentity $ runStateT f value
  writeTVar var new
  return (v, new)
