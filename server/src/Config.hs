{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import           Control.Lens
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Config =
  Config {_address :: Text
         ,_port    :: Int}
  deriving (Show,Eq,Generic,FromJSON)

makeLenses ''Config

defaultConfig :: Config
defaultConfig =
  Config {_address = "127.0.0.1"
         ,_port = 8080}
