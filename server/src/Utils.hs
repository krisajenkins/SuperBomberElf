module Utils where

import           Control.Concurrent
import           Data.Time

threadPause :: NominalDiffTime -> IO ()
threadPause = threadDelay . floor . (1000 * 1000 *) . toRational

singleton :: a -> [a]
singleton x = [x]

addTime :: Int -> UTCTime -> UTCTime
addTime d = addUTCTime (fromIntegral d)
