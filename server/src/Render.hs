{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Render where

import           Control.Lens
import           Data.Aeson
import qualified Data.Map     as Map
import           Data.Maybe
import           Types

displayPosition :: Position -> Value
displayPosition Position {..} = object [("x", toJSON _x), ("y", toJSON _y)]

displayPlayer :: ClientId -> Player -> Value
displayPlayer playerId Player {..} =
  object
    [ ("id", toJSON playerId)
    , ("name", toJSON _playerName)
    , ("position", displayPosition _playerPosition)
    , ("alive", toJSON (isNothing _playerDiedAt))
    , ("score", toJSON _playerScore)
    ]

displayBomb :: Bomb -> Value
displayBomb bomb =
  object
    [ ("position", displayPosition (view bombPosition bomb))
    , ("blast", toJSON (view blast bomb))
    ]

displayWall :: Wall -> Value
displayWall Wall {..} =
  object
    [ ("position", displayPosition _wallPosition)
    , ("alive", toJSON (isNothing _wallDiedAt))
    , ("type", toJSON _wallType)
    ]

displayScene :: Scene -> Value
displayScene s =
  object
    [ ("bombs", toJSON (displayBomb <$> view bombs s))
    , ("walls", toJSON (displayWall <$> view walls s))
    , ( "players"
      , toJSON
          (Map.foldrWithKey (\k v b -> displayPlayer k v : b) [] (view players s)))
    ]
