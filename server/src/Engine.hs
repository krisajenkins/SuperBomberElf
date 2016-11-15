{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Engine
  ( handleGameEvent
  ) where

import           Config
import           Control.Lens
import           Control.Monad.Writer
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Set.Lens
import           Data.Time
import           Types

dec
  :: Num a
  => a -> a
dec n = n - 1

------------------------------------------------------------
handlePlayerCommand :: ClientId -> PlayerCommand -> Scene -> Scene
handlePlayerCommand playerId (Move direction) scene =
  over (players . ix playerId) moveIfYouCan scene
  where
    moveIfYouCan player =
      if canMove scene player direction
        then over playerPosition (stepIn direction) player
        else player
handlePlayerCommand playerId (SetName s) scene =
  set (players . ix playerId . playerName) s scene
handlePlayerCommand _ Look scene = scene
handlePlayerCommand playerId DropBomb scene =
  case view (players . at playerId) scene of
    Nothing -> scene
    Just player ->
      if isDead now player
        then scene
        else let newBomb =
                   Bomb
                   { _bombPosition = view playerPosition player
                   , _blast = Nothing
                   , _bombOwner = playerId
                   , _bombExplodesAt =
                     addUTCTime (fromRational 3) (view clock scene)
                   }
             in over bombs (newBomb :) scene
  where
    now = view clock scene

canMove :: Scene -> Player -> Direction -> Bool
canMove scene player direction =
  let newPosition = stepIn direction (view playerPosition player)
      now = view clock scene
  in if | isDead now player -> False
        | isLiveWallAt scene newPosition -> False
        | isBombAt scene newPosition -> False
        | otherwise -> True

isLiveWallAt :: Scene -> Position -> Bool
isLiveWallAt scene position = Set.member position (allLiveWallPositions scene)

isBombAt :: Scene -> Position -> Bool
isBombAt scene position = Set.member position (allBombPositions scene)

handleGameEvent :: GameEvent -> Scene -> Scene
handleGameEvent (FromPlayer clientId command) = handlePlayerCommand clientId command
handleGameEvent (Tick time) =
  set clock time .
  removeDeadBombs .
  respawnWalls . respawnPlayers . detonateBombs . killWalls . killPlayers

allBombPositions :: Scene -> Set Position
allBombPositions = setOf (bombs . traverse . bombPosition)

allLiveWallPositions :: Scene -> Set Position
allLiveWallPositions scene =
  setOf (walls . traverse . filtered (not . isDead now) . wallPosition) scene
  where
    now = view clock scene

allBlastPositions :: Scene -> Set Position
allBlastPositions scene = foldl reducer Set.empty (view bombs scene)
  where
    reducer positions bomb = Set.union positions (blastSite bomb)

respawnWalls :: Scene -> Scene
respawnWalls scene = over (walls . traverse) (maybeRespawn time) scene
  where
    time = view clock scene

respawnPlayers :: Scene -> Scene
respawnPlayers scene = over (players . traverse) (maybeRespawn time) scene
  where
    time = view clock scene

detonateBombs :: Scene -> Scene
detonateBombs scene = over (bombs . traverse) detonateBomb scene
  where
    now = view clock scene
    wallPositions = allLiveWallPositions scene
    isDetonated bomb = isJust (view blast bomb)
    detonateBomb bomb =
      if | isDetonated bomb -> bomb
         | view bombExplodesAt bomb > now -> bomb
         | otherwise ->
           set
             blast
             (Just
                (Blast
                   (Map.fromList
                      (blastRadius (view bombPosition bomb) <$> [minBound ..]))))
             bomb
    blastRadius position direction = (direction, radius)
      where
        steps = iterate (stepIn direction) position
        wallAtStep = fmap (`Set.member` wallPositions) steps
        clearSteps :: [Bool] = takeWhile not wallAtStep
        radius = min blastSize (length clearSteps)

blastSite :: Bomb -> Set Position
blastSite bomb =
  Set.fromList $
  Map.foldWithKey reducer [] (fromMaybe Map.empty (unBlast <$> view blast bomb))
  where
    reducer direction count p =
      p <>
      take (count + 1) (iterate (stepIn direction) (view bombPosition bomb))

removeDeadBombs :: Scene -> Scene
removeDeadBombs scene = over bombs (filter (not . isDead now)) scene
  where
    now = view clock scene

killWalls :: Scene -> Scene
killWalls scene = over (walls . traverse) maybeKill scene
  where
    now = view clock scene
    blastPositions = allBlastPositions scene
    maybeKill wall =
      if | view wallType wall == Strong -> wall
         | isDead now wall -> wall
         | Set.member (view wallPosition wall) blastPositions ->
           set wallDiedAt (Just now) wall
         | otherwise -> wall

killPlayers :: Scene -> Scene
killPlayers scene = over (players . traverse) maybeKill scene
  where
    now = view clock scene
    blastPositions = allBlastPositions scene
    maybeKill player =
      if | isDead now player -> player
         | Set.member (view playerPosition player) blastPositions ->
           (over playerScore dec . set playerDiedAt (Just now)) player
         | otherwise -> player
