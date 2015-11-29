{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Events
       (Event(..), Scene(..), Player(..), Time, Reaction(..), sceneAt,
        scheduleAt)
       where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Aeson
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Text           as T
import           Data.Time
import           Data.UUID
import           GHC.Generics
import           Utils

data GameConfig = GameConfig {fuseTime :: NominalDiffTime}

type Time = UTCTime

type Schedule = Map Time [Event]

instance ToJSON UUID where
  toJSON uuid = String . T.pack $ show uuid

data Event
  = AddPlayer UUID
  | SetPlayerName UUID
                  (Maybe String)
  | RemovePlayer UUID
  deriving (Show,Eq)

data Reaction =
  PlayerAlreadyAdded UUID
  deriving (Show,Eq)

data Player =
  Player {_playerName :: Maybe String}
  deriving (Show,Eq,Generic,ToJSON)
makeLenses ''Player

data Scene =
  Scene {_players :: Map UUID Player}
  deriving (Show,Eq)
makeLenses ''Scene

scheduleAt :: Schedule -> [(Event, Time)] -> Schedule
scheduleAt = foldl (\s (e,t) -> Map.insertWith (<>) t [e] s)

reaction :: MonadWriter [a] m => a -> m ()
reaction e = tell [e]

handleEventRWS :: Event -> RWS GameConfig [Reaction] Scene ()
handleEventRWS (AddPlayer uuid) =
  do player <- use (players . at uuid)
     case player of
       Just _ -> reaction (PlayerAlreadyAdded uuid)
       Nothing ->
         assign (players . at uuid)
                (Just initialPlayer)
handleEventRWS (SetPlayerName uuid name) =
  assign (players . ix uuid . playerName) name
handleEventRWS (RemovePlayer uuid) = assign (players . at uuid) Nothing

handleEvent :: ([Reaction], Scene) -> Event -> ([Reaction], Scene)
handleEvent (rs,s) (AddPlayer uuid) =
  if Map.member uuid
                (view players s)
     then (PlayerAlreadyAdded uuid : rs,s)
     else (rs ,set (players . at uuid)
               (Just initialPlayer)
               s)
handleEvent (rs,s) (RemovePlayer uuid) = (rs,set (players . at uuid) Nothing s)
handleEvent (rs,s) (SetPlayerName uuid name) =
  (rs,set (players . ix uuid . playerName) name s)

initialScene :: Scene
initialScene = Scene {_players = Map.empty}

initialPlayer :: Player
initialPlayer = Player {_playerName = Nothing}

sceneAt :: Schedule -> Time -> (Scene,[Reaction])
sceneAt s t =
  execRWS (sceneAtRWS s t)
          GameConfig {fuseTime = 3.0}
          initialScene

sceneAtRWS :: Schedule -> Time -> RWS GameConfig [Reaction] Scene ()
sceneAtRWS schedule t = mapM_ handleEventRWS events
  where (events,_) = dueEvents t schedule

dueEvents :: Time -> Schedule -> ([Event], Schedule)
dueEvents t schedule = (due,after)
  where (before,during,after) = Map.splitLookup t schedule
        due = mconcat (Map.elems before <> maybe [] singleton during)
