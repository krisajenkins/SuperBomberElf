{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where

import           Control.Lens (Lens', makeLenses)
import           Data.Aeson
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Text    as T
import           Data.Time
import           Data.UUID

newtype ClientId =
  ClientId UUID
  deriving (Show,Eq,Ord)

instance ToJSON ClientId where
  toJSON (ClientId uuid) = String . T.pack $ show uuid

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
  Player {_playerName     :: Maybe Text
         ,_playerPosition :: Position}
makeLenses ''Player

data Scene =
  Scene {_players :: Map ClientId Player
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
    object ["players" .= (Map.elems _players),"walls" .= _walls,"bombs" .= _bombs]

data PlayerCommand
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DropBomb
  deriving (Eq,Show)

data ServerMessage
  = PlayerMessage PlayerCommand (Lens' Scene Player)
  | Tick UTCTime

initialScene :: UTCTime -> Scene
initialScene _clock =
  let _players = Map.empty
      _walls = []
      _bombs = []
  in Scene {..}
