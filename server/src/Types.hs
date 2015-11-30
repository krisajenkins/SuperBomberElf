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

import           Config
import           Control.Lens (makeLenses, over, view)
import           Data.Aeson
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text    as T
import           Data.Time
import           Data.UUID
import qualified Data.Vector  as V
import           GHC.Generics

newtype ClientId =
  ClientId {unCliendId :: UUID}
  deriving (Show,Eq,Ord)

instance ToJSON ClientId where
  toJSON (ClientId uuid) = String . T.pack $ show uuid

data Position =
  Position {_x :: Int
           ,_y :: Int}
  deriving (Show,Eq,Ord)
makeLenses ''Position

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position x1 y1) (Position x2 y2) =
    Position (x1 + x2)
             (y1 + y2)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq,Ord,Bounded,Enum,Show,Generic,FromJSON,ToJSON)

toPosition :: Direction -> Position
toPosition North = Position {_x = 0, _y = -1}
toPosition South = Position {_x = 0, _y = 1}
toPosition West = Position {_x = -1, _y = 0}
toPosition East = Position {_x = 1, _y = 0}

stepIn :: Direction -> Position -> Position
stepIn = mappend . toPosition

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
       ,_bombOwner      :: ClientId
       ,_blast          :: Maybe Blast}
makeLenses ''Bomb

data Player =
  Player {_playerName     :: Text
         ,_playerDiedAt   :: Maybe UTCTime
         ,_playerPosition :: Position
         ,_playerScore    :: Int}
  deriving (Show)
makeLenses ''Player

data Scene =
  Scene {_players :: Map ClientId Player
        ,_walls   :: [Wall]
        ,_bombs   :: [Bomb]
        ,_clock   :: UTCTime}
makeLenses ''Scene

data PlayerCommand
  = DropBomb
  | SetName Text
  | Move Direction
  deriving (Eq,Show,Generic)

allPlayerCommands :: Text -> [PlayerCommand]
allPlayerCommands name = DropBomb : SetName name : (Move <$> [minBound ..])

instance ToJSON PlayerCommand where
  toJSON DropBomb = object [("command",toJSON $ show DropBomb)]
  toJSON (SetName name) = object [("command",toJSON ("SetName" :: Text,name))]
  toJSON (Move d) = object [("command",toJSON $ "Move" <> show d)]

instance FromJSON PlayerCommand where
  parseJSON (Object o) = o .: "command"
  parseJSON (Array (V.toList -> [String "SetName",String s])) = pure $ SetName s
  parseJSON (String "DropBomb") = pure DropBomb
  parseJSON (String (T.splitAt 4 -> ("Move",dir))) =
    Move <$> parseJSON (String dir)
  parseJSON _ = fail "Invalid command."

data GameEvent
  = FromPlayer ClientId PlayerCommand
  | Tick UTCTime

respawn :: UTCTime -> NominalDiffTime -> Maybe UTCTime -> Maybe UTCTime
respawn _ _  Nothing = Nothing
respawn now respawnDelay (Just d) =
  if addUTCTime respawnDelay d < now
     then Nothing
     else Just d

------------------------------------------------------------

class LivingThing a where
  isDead :: UTCTime -> a -> Bool
  maybeRespawn :: UTCTime -> a -> a

instance LivingThing Bomb where
  isDead now bomb = (\t -> now > addUTCTime blastDelay t) $ view bombExplodesAt bomb
  maybeRespawn _ bomb = bomb

instance LivingThing Wall where
  isDead now wall = fromMaybe False $ (now >) <$> view wallDiedAt wall
  maybeRespawn now = over wallDiedAt (respawn now wallRespawnDelay)

instance LivingThing Player where
  isDead now player = fromMaybe False $ (now >) <$> view playerDiedAt player
  maybeRespawn now = over playerDiedAt (respawn now playerRespawnDelay)
