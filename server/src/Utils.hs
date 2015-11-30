module Utils where

import           Control.Concurrent
import           Data.Aeson
import           Data.Time
import qualified Network.WebSockets as WS

threadPause :: NominalDiffTime -> IO ()
threadPause = threadDelay . floor . (1000 * 1000 *) . toRational

singleton :: a -> [a]
singleton x = [x]

addTime :: Int -> UTCTime -> UTCTime
addTime d = addUTCTime (fromIntegral d)

toMessage :: ToJSON a => a -> WS.Message
toMessage = WS.DataMessage . WS.Text . encode
