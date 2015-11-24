{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Types where

import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Monoid
import           Data.Text    as T
import           Data.Time
import           Data.UUID
import           GHC.Generics

newtype ClientId =
  ClientId UUID
  deriving (Show,Eq,Ord)

instance ToJSON ClientId where
  toJSON (ClientId uuid) = String . T.pack $ show uuid

data Position =
  Position {_x :: Int
           ,_y :: Int}
  deriving (Show,Eq,Ord)
makeLenses ''Position

data WallType
  = Strong
  | Weak
  deriving (Show,Eq,Generic,ToJSON)
makeLenses ''WallType

data Wall =
  Wall {_wallType     :: WallType
       ,_wallPosition :: Position}
makeLenses ''Wall

data Bomb =
  Bomb {_bombPosition  :: Position
       ,_bombDroppedAt :: UTCTime}
makeLenses ''Bomb

data DisplayBomb =
  DisplayBomb {_displayBombPosition :: Position
               ,_displayBombRadius  :: Int}
  deriving (Show,Eq,Generic)
makeLenses ''DisplayBomb

data Player =
  Player {_playerName     :: Maybe Text
         ,_playerPosition :: Position}
makeLenses ''Player

data Scene =
  Scene {_players :: Map ClientId Player
        ,_walls   :: [Wall]
        ,_bombs   :: [Bomb]
        ,_clock   :: UTCTime}
makeLenses ''Scene

data PlayerCommand
  = DropBomb
  | North
  | South
  | East
  | West
  deriving (Eq,Bounded,Enum,Show,Generic,FromJSON,ToJSON)

newtype PlayerMessage = PlayerMessage { message :: PlayerCommand}
  deriving (Eq,Show,Generic,FromJSON,ToJSON)

data ServerCommand
  = FromPlayer ClientId PlayerCommand
  | Tick UTCTime

initialScene :: UTCTime -> Scene
initialScene _clock =
  let _walls =
        (wallAt Strong . (,0) <$> [0 .. 10]) <>
        (wallAt Strong . (,10) <$> [0 .. 10]) <>
        (wallAt Strong . (0,) <$> [0 .. 10]) <>
        (wallAt Strong . (10,) <$> [0 .. 10]) <>
        (wallAt Weak . (3,) <$> [1 .. 9]) <>
        (wallAt Weak . (7,) <$> [1 .. 9]) <>
        (wallAt Weak . (,3) <$> [1 .. 9]) <>
        (wallAt Weak . (,7) <$> [1 .. 9])
      _players = Map.empty
      _bombs = []
  in Scene {..}
  where wallAt wt (wx,wy) =
          Wall {_wallType = wt
               ,_wallPosition = Position wx wy}
