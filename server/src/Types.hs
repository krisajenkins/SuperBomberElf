{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
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

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq,Ord,Bounded,Enum,Show,Generic,FromJSON,ToJSON)

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
       ,_wallDiedAt   :: Maybe UTCTime
       ,_wallPosition :: Position}
makeLenses ''Wall

newtype Blast =
  Blast {unBlast :: Map Direction Int}
  deriving (Show,Eq)

instance ToJSON Blast where
  toJSON = toJSON . Map.mapKeys show . unBlast

data Bomb =
  Bomb {_bombPosition   :: Position
       ,_bombExplodesAt :: UTCTime
        ,_bombOwner     :: ClientId
       ,_blast          :: Maybe Blast}
makeLenses ''Bomb

data Player =
  Player {_playerName     :: Maybe Text
         ,_playerDiedAt   :: Maybe UTCTime
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
  | Move Direction
  deriving (Eq,Show,Generic)

instance ToJSON PlayerCommand where
  toJSON DropBomb = object [("command" , toJSON $ show DropBomb)]
  toJSON (Move d) = object [("command" , toJSON $ "Move" <> show d)]

instance FromJSON PlayerCommand where
  parseJSON (Object o) = o .: "command"
  parseJSON (String "DropBomb") = pure DropBomb
  parseJSON (String (T.splitAt 4 -> ("Move", dir))) = Move <$> parseJSON (String dir)
  parseJSON _ = fail "Invalid command."

allPlayerCommands :: [PlayerCommand]
allPlayerCommands = DropBomb : allMoves
  where allMoves = Move <$> [minBound ..]

data ServerCommand
  = FromPlayer ClientId PlayerCommand
  | Tick UTCTime
