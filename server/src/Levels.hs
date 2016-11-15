{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Levels where

import qualified Data.Map    as Map
import           Data.Monoid
import           Data.Time
import           Types

wallAt :: WallType -> (Int, Int) -> Wall
wallAt wt (wx, wy) =
  Wall
  { _wallType = wt
  , _wallDiedAt = Nothing
  , _wallPosition = Position wx wy
  }

outerWalls :: Int -> Int -> [Wall]
outerWalls maxX maxY =
  (wallAt Strong . (, 0) <$> [0 .. maxX]) <>
  (wallAt Strong . (, maxY) <$> [0 .. maxX]) <>
  (wallAt Strong . (0, ) <$> [0 .. maxY]) <>
  (wallAt Strong . (maxX, ) <$> [0 .. maxY])

poundWalls :: [Wall]
poundWalls =
  outerWalls 10 10 <> (wallAt Weak . (3, ) <$> [1 .. 9]) <>
  (wallAt Weak . (7, ) <$> [1 .. 9]) <>
  (wallAt Weak . (, 3) <$> [1 .. 9]) <>
  (wallAt Weak . (, 7) <$> [1 .. 9]) <>
  (wallAt Weak <$> [(1, 5), (5, 1), (9, 5), (5, 9)]) <>
  (wallAt Weak <$> [(2, 2), (2, 8), (8, 2), (8, 8)]) <>
  (wallAt Strong <$> [(3, 3), (7, 3), (3, 7), (7, 7)]) <>
  [wallAt Strong (5, 5)]

simpleWalls :: [Wall]
simpleWalls =
  outerWalls 10 10 <>
  (wallAt Strong <$>
   do a <- [2, 4, 6, 8]
      b <- [2, 4, 6, 8]
      return (a, b)) <>
  (wallAt Weak . (, 1) <$> [3, 7]) <>
  (wallAt Weak . (, 5) <$> [3, 7]) <>
  (wallAt Weak . (, 9) <$> [3, 7]) <>
  (wallAt Weak . (, 3) <$> [1, 5, 9]) <>
  (wallAt Weak . (, 7) <$> [1, 5, 9])

validStartPositions :: [Position]
validStartPositions = do
  a <- [1, 9]
  b <- [1, 9]
  return (Position a b)

initialScene :: UTCTime -> Scene
initialScene _clock =
  let _walls = simpleWalls
      _players = Map.empty
      _bombs = []
  in Scene
     { ..
     }
