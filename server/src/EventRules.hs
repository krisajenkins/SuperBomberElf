{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module EventRules
       (Event(..),PlayerEvent(..), Scene(..), Player(..),playerName, Time, Reaction(..), sceneAt,
        scheduleFrom,initialPlayer)
       where

import           Control.Lens
import           Control.Monad.RWS
-- import           Data.Aeson
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Time
import           Data.UUID
import           EventSystem
import           GHC.Generics
import           Types             (Position (..))

data GameConfig = GameConfig {fuseTime :: NominalDiffTime}

data PlayerEvent
  = Join
  | SetName (Maybe String)
  | Leave
  deriving (Show,Eq)

data Event =
  FromPlayer UUID
             PlayerEvent
  deriving (Show,Eq)

data Reaction =
  PlayerAlreadyAdded UUID
  deriving (Show,Eq)

data Player =
  Player {_playerName     :: Maybe String
         ,_playerAlive    :: Bool
         ,_playerScore    :: Int
         ,_playerPosition :: Position}
  deriving (Show,Eq,Generic)
makeLenses ''Player

data Scene =
  Scene {_players :: Map UUID Player}
  deriving (Show,Eq)
makeLenses ''Scene

handleEvent :: Event -> RWS GameConfig [Reaction] Scene ()
handleEvent (FromPlayer uuid Join) =
  do player <- use (players . at uuid)
     case player of
       Just _ -> reaction (PlayerAlreadyAdded uuid)
       Nothing ->
         assign (players . at uuid)
                (Just (initialPlayer (Position 1 1)))
handleEvent (FromPlayer uuid (SetName name)) =
  assign (players . ix uuid . playerName) name
handleEvent (FromPlayer uuid Leave) = assign (players . at uuid) Nothing

initialScene :: Scene
initialScene = Scene {_players = Map.empty}

initialPlayer :: Position -> Player
initialPlayer initialPosition =
  Player {_playerName = Nothing
         ,_playerAlive = True
         ,_playerPosition = initialPosition
         ,_playerScore = 0}

sceneAt :: Time -> Schedule Event -> (Scene,[Reaction])
sceneAt t s =
  execRWS (sceneAtRWS t s)
          GameConfig {fuseTime = 3.0}
          initialScene

sceneAtRWS :: Time -> Schedule Event -> RWS GameConfig [Reaction] Scene ()
sceneAtRWS t = mapM_ handleEvent . fst . dueEvents t
