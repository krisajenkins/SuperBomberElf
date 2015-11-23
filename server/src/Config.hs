{-# LANGUAGE TemplateHaskell #-}
module Config where

import           Control.Lens

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
