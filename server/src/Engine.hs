{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Engine (update,blastRadius,wallExpired,bombExpired) where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set     as Set
import           Data.Time
import           Safe         (atMay)
import           Types

inc, dec :: Int -> Int
inc n = n + 1
dec n = n - 1

playerPositionAt :: ClientId -> Traversal' Scene Position
playerPositionAt playerId = players . ix playerId . playerPosition

handlePlayerCommand :: ClientId -> PlayerCommand -> Scene -> Scene
handlePlayerCommand playerId North scene = over (playerPositionAt playerId . y) dec scene
handlePlayerCommand playerId South scene = over (playerPositionAt playerId . y) inc scene
handlePlayerCommand playerId West  scene = over (playerPositionAt playerId . x) dec scene
handlePlayerCommand playerId East  scene = over (playerPositionAt playerId . x) inc scene
handlePlayerCommand playerId DropBomb scene =
  case view (players . at playerId) scene of
    Nothing -> scene
    Just player ->
      let _bombPosition = view playerPosition player
          _bombDroppedAt = view clock scene
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
update (Tick t) scene = scene {_clock = t}
update (FromPlayer clientId command) scene =
  if validScene newScene
     then newScene
     else scene
  where newScene = handlePlayerCommand clientId command scene

wallExpired :: UTCTime -> Wall -> Bool
wallExpired t wall =
  case view wallDiedAt wall of
    Nothing -> False
    Just died -> died < t

bombExpired :: UTCTime -> Bomb -> Bool
bombExpired t = isNothing . blastRadius t . view bombDroppedAt

blastPattern :: [(Int,Int)]
blastPattern =
  zip [0 ..]
      (replicate 20 0 <> ([1 .. 5] <> [5,4 .. 1]))

blastRadius :: UTCTime -> UTCTime -> Maybe Int
blastRadius currentTime droppedTime =
  let age :: Int = floor . (*10). toRational $ diffUTCTime currentTime droppedTime
  in snd <$> atMay (concatMap (replicate 1) blastPattern ) age
