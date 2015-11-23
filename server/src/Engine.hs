{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Engine where

import           Control.Lens
import           Types

inc, dec :: Int -> Int
inc = (+) 1
dec = (-) 1

handlePlayerCommand :: PlayerCommand -> Lens' Scene Player -> Scene -> Scene
handlePlayerCommand MoveUp player scene = over (player . playerPosition . y) dec scene
handlePlayerCommand MoveDown player scene = over (player . playerPosition . y) inc scene
handlePlayerCommand MoveLeft player scene = over (player . playerPosition . x) dec scene
handlePlayerCommand MoveRight player scene = over (player . playerPosition . x) inc scene
handlePlayerCommand DropBomb player scene =
  let _bombPosition = view (player . playerPosition) scene
      _droppedAt = view clock scene
      newBomb = Bomb {..}
  in over bombs (newBomb :) scene
