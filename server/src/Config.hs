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
fuseTime ::  NominalDiffTime
fuseTime = fromRational 2.0

blastTime :: NominalDiffTime
blastTime = fromRational 0.5

blastSize :: Int
blastSize = 3

respawnTime :: NominalDiffTime
respawnTime = fromRational 5.0
