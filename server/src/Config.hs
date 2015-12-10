{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import           AesonUtils
import           Control.Lens
import           Data.Aeson.TH    (deriveJSON)
import           Data.Monoid
import           Data.Time
import           Data.Yaml
import           GHC.Generics
import           System.Directory

data BindTo =
  BindTo {_address :: String
         ,_port    :: Int}
  deriving (Show,Eq,Generic)
makeLenses ''BindTo

$(deriveJSON (dropPrefixJSONOptions "_") ''BindTo)

data Config =
  Config {_playersBindTo :: BindTo
         ,_ekgBindsTo    :: BindTo
         ,_staticDir     :: FilePath}
  deriving (Show,Eq,Generic)
makeLenses ''Config

$(deriveJSON (dropPrefixJSONOptions "_") ''Config)

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

loadConfig :: IO (Either ParseException Config)
loadConfig =
  do homeDirectory <- getHomeDirectory
     let configFile = homeDirectory <> "/.bomberman.yaml"
     putStrLn $ "Reading config: " <> configFile
     decodeFileEither configFile
