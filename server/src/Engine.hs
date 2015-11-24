{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Engine (update) where

import           Control.Lens
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
          _droppedAt = view clock scene
          newBomb = Bomb {..}
      in over bombs (newBomb :) scene

update :: ServerCommand -> Scene -> Scene
update (FromPlayer clientId command) scene = handlePlayerCommand clientId command scene
update (Tick t) scene =
  -- TODO Expire bombs.
  set clock t scene
