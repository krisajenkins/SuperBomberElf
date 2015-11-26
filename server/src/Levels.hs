{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Levels where

import qualified Data.Map    as Map
import           Data.Monoid
import           Data.Time
import           Types

wallAt :: WallType -> (Int, Int) -> Wall
wallAt wt (wx,wy) =
          Wall {_wallType = wt
               ,_wallDiedAt = Nothing
               ,_wallPosition = Position wx wy}

outerWalls :: [Wall]
outerWalls =
  (wallAt Strong . (,0) <$> [0 .. 10]) <> (wallAt Strong . (,10) <$> [0 .. 10]) <>
  (wallAt Strong . (0,) <$> [0 .. 10]) <>
  (wallAt Strong . (10,) <$> [0 .. 10])

poundWalls :: [Wall]
poundWalls =
  outerWalls <> (wallAt Weak . (3,) <$> [1 .. 9]) <>
  (wallAt Weak . (7,) <$> [1 .. 9]) <>
  (wallAt Weak . (,3) <$> [1 .. 9]) <>
  (wallAt Weak . (,7) <$> [1 .. 9]) <>
  (wallAt Weak <$> [(1,5),(5,1),(9,5),(5,9)]) <>
  (wallAt Weak <$> [(2,2),(2,8),(8,2),(8,8)]) <>
  (wallAt Strong <$> [(3,3),(7,3),(3,7),(7,7)]) <>
  [wallAt Strong (5,5)]

simpleWalls :: [Wall]
simpleWalls =
  outerWalls <>
  (wallAt Strong <$>
   do a <- [2,4,6,8]
      b <- [2,4,6,8]
      return (a,b)) <>
  (wallAt Weak . (,1) <$> [3,7]) <>
  (wallAt Weak . (,5) <$> [3,7]) <>
  (wallAt Weak . (,9) <$> [3,7]) <>
  (wallAt Weak . (,3) <$> [1,5,9]) <>
  (wallAt Weak . (,7) <$> [1,5,9])

initialScene :: UTCTime -> Scene
initialScene _clock =
  let _walls = poundWalls
      _players = Map.empty
      _bombs = []
  in Scene {..}
