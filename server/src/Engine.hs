{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Engine (update) where

import           Config
import           Control.Lens
import qualified Data.Map     as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Time
import           Types

inc, dec :: Int -> Int
inc n = n + 1
dec n = n - 1

handlePlayerCommand :: ClientId -> PlayerCommand -> Scene -> Scene
handlePlayerCommand playerId (Move direction) scene =
  over (players . ix playerId) moveIfYouCan scene
  where moveIfYouCan player =
          if playerExpired (view clock scene)
                           player
             then player
             else over playerPosition (stepIn direction) player
handlePlayerCommand playerId DropBomb scene =
  case view (players . at playerId) scene of
    Nothing -> scene
    Just player ->
      let _bombPosition = view playerPosition player
          _blast = Nothing
          _bombOwner = playerId
          _bombExplodesAt =
            addUTCTime (fromRational 3)
                       (view clock scene)
          newBomb = Bomb {..}
      in over bombs (newBomb :) scene

validScene :: Scene -> Bool
validScene scene =
  let playerPositions =
        Set.fromList $ toListOf (players . each . playerPosition) scene
      wallPositions =
        Set.fromList $ toListOf (walls . each . wallPosition) scene
  in Set.null (playerPositions `Set.intersection` wallPositions)

update :: ServerCommand -> Scene -> Scene
update (Tick t) scene =
  (set clock t .
   removeDeadWalls .
   removeDeadBombs .
   respawnDeadPlayers . detonateBombs . killWalls . killPlayers) scene
update (FromPlayer clientId command) scene =
  if validScene newScene
     then newScene
     else scene
  where newScene = handlePlayerCommand clientId command scene


stepIn :: Direction -> Position -> Position
stepIn West = over x dec
stepIn East = over x inc
stepIn North = over y dec
stepIn South = over y inc

removeDeadWalls :: Scene -> Scene
removeDeadWalls scene = over walls (filter isWallAlive) scene
  where now = view clock scene
        isWallAlive wall =
          case view wallDiedAt wall of
            Nothing -> True
            Just t -> t > now

removeDeadBombs :: Scene -> Scene
removeDeadBombs scene = over bombs (filter isBombAlive) scene
  where now = view clock scene
        isBombAlive bomb =
          now <
          addUTCTime (fuseTime + blastTime)
                     (view bombExplodesAt bomb)

respawnDeadPlayers :: Scene -> Scene
respawnDeadPlayers scene = over (players . traverse . playerDiedAt) f scene
  where t = view clock scene
        f Nothing = Nothing
        f (Just d) =
          if addUTCTime respawnTime d < t
             then Nothing
             else Just d

allWallPositions :: Scene -> Set Position
allWallPositions scene = Set.fromList $ toListOf (walls . traverse . wallPosition) scene

detonateBombs :: Scene -> Scene
detonateBombs scene = over (bombs . traverse) detonateBomb scene
  where now = view clock scene
        wallPositions = allWallPositions scene
        isDetonated bomb = isJust (view blast bomb)
        detonateBomb bomb =
          if |  isDetonated bomb -> bomb
             |  view bombExplodesAt bomb > now -> bomb
             |  otherwise ->
               set blast
                   (Just (Blast (Map.fromList
                                   (blastRadius (view bombPosition bomb) <$>
                                    [minBound ..]))))
                   bomb
        blastRadius position direction = (direction,radius)
          where steps = iterate (stepIn direction) position
                wallAtStep = fmap (`Set.member` wallPositions) steps
                clearSteps :: [Bool] = takeWhile not wallAtStep
                radius = min blastSize (length clearSteps)

blastSite :: Bomb -> Set Position
blastSite bomb =
  Set.fromList $
  Map.foldWithKey reducer
                  [center]
                  (fromMaybe Map.empty (unBlast <$> view blast bomb))
  where center = view bombPosition bomb
        reducer direction count p =
          p <> take count (tail (iterate (stepIn direction) center))

allBlastPositions :: Scene -> Set Position
allBlastPositions scene = foldl reducer Set.empty (view bombs scene)
  where reducer positions bomb =
          Set.union positions (blastSite bomb)


killWalls :: Scene -> Scene
killWalls scene = over (walls . traverse) maybeKill scene
  where now = view clock scene
        blastPositions = allBlastPositions scene
        maybeKill wall =
          if |  view wallType wall == Strong -> wall
             |  isJust (view wallDiedAt wall) -> wall
             |  Set.member (view wallPosition wall)
                           blastPositions -> set wallDiedAt (Just now) wall
             |  otherwise -> wall

killPlayers :: Scene -> Scene
killPlayers scene = scene

playerExpired :: UTCTime -> Player -> Bool
playerExpired t player =
  case view playerDiedAt player of
    Nothing -> False
    Just died -> died < t
