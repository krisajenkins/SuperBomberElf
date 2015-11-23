{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Text
import           Data.Time

data Position =
  Position {_x :: Int
           ,_y :: Int}
  deriving (Show,Eq)
makeLenses ''Position

data Wall =
  Wall {_wallPosition :: Position}
makeLenses ''Wall

data Bomb =
  Bomb {_bombPosition :: Position
       ,_droppedAt    :: UTCTime}
makeLenses ''Bomb

data Player =
  Player {_playerName     :: Text
         ,_playerPosition :: Position}
makeLenses ''Player

data Scene =
  Scene {_players :: [Player]
        ,_walls   :: [Wall]
        ,_bombs   :: [Bomb]
        ,_clock   :: UTCTime}
makeLenses ''Scene

instance ToJSON Position where
  toJSON Position{..} = object ["x" .= _x,"y" .= _y]

instance ToJSON Wall where
  toJSON Wall{..} = object ["position" .= _wallPosition]

instance ToJSON Bomb where
  toJSON Bomb{..} = object ["position" .= _bombPosition]

instance ToJSON Player where
  toJSON Player{..} = object ["name" .= _playerName,"position" .= _playerPosition]

instance ToJSON Scene where
  toJSON Scene{..} =
    object ["players" .= _players,"walls" .= _walls,"bombs" .= _bombs]

data PlayerCommand
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DropBomb
  deriving (Eq,Show)
