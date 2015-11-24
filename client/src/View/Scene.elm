module View.Scene where

import Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

wallColor : WallType -> String
wallColor t =
  case t of
    Strong -> "teal"
    Weak -> "lightsalmon"

blockSize : Int
blockSize  = 30

wallView : Wall -> Svg
wallView wall =
  rect [x (toString (wall.position.x * blockSize))
       ,y (toString (wall.position.y * blockSize))
       ,width (toString blockSize)
       ,height (toString blockSize)
       ,fill (wallColor wall.wallType)
       ,stroke "white"]
       []

playerView : Player -> Svg
playerView player =
  circle [cx ((player.position.x * blockSize) + (blockSize // 2) |> toString)
         ,cy ((player.position.y * blockSize) + (blockSize // 2) |> toString)
         ,r (toString (blockSize // 2))]
         []

sceneView : Scene -> Svg
sceneView scene =
  svg [width (toString (blockSize * 11))
      ,height (toString (blockSize * 11))]
      ((List.map wallView scene.walls)
       ++
       (List.map playerView scene.players))
