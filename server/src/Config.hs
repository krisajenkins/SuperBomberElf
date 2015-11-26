{-# LANGUAGE TemplateHaskell #-}
module Config where

import           Control.Lens
import           Data.Time

data BindTo =
  BindTo {_address :: String
         ,_port    :: Int}
  deriving (Show,Eq)

makeLenses ''BindTo

data Config =
  Config {_playersBindTo :: BindTo}
  deriving (Show,Eq)

makeLenses ''Config

defaultConfig :: Config
defaultConfig =
  Config {_playersBindTo =
            BindTo {_address = "127.0.0.1"
                   ,_port = 8080}}

-- TODO Organise:
fuseDelay ::  NominalDiffTime
fuseDelay = fromRational 2.0

blastDelay :: NominalDiffTime
blastDelay = fromRational 0.5

blastSize :: Int
blastSize = 3

playerRespawnDelay :: NominalDiffTime
playerRespawnDelay = fromRational 5.0

wallRespawnDelay :: NominalDiffTime
wallRespawnDelay = fromRational 30.0

playerThrottleDelay :: NominalDiffTime
playerThrottleDelay = fromRational 0.2

frameDelay :: NominalDiffTime
frameDelay = fromRational 0.1
