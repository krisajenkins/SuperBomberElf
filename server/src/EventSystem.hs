{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module EventSystem where

import           Control.Monad.RWS
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Time
import           Utils

type Time = UTCTime

type Schedule e = Map Time [e]

scheduleFrom :: Schedule e -> [(e, Time)] -> Schedule e
scheduleFrom = foldl (\s (e,t) -> Map.insertWith (<>) t [e] s)

reaction :: MonadWriter [a] m => a -> m ()
reaction e = tell [e]

dueEvents :: Time -> Schedule e -> ([e], Schedule e)
dueEvents t schedule = (due,after)
  where (before,during,after) = Map.splitLookup t schedule
        due = mconcat (Map.elems before <> maybe [] singleton during)
