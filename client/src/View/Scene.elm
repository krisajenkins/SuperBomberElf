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
blockSize  = 50

wallView : Wall -> Svg
wallView wall =
  rect [x (toString (wall.position.x * blockSize))
       ,y (toString (wall.position.y * blockSize))
       ,width (toString blockSize)
       ,height (toString blockSize)
       ,fill (wallColor wall.wallType)
       ,stroke "white"]
       []

playerColor : Player -> String
playerColor _ = "blueviolet"

bombView : Bomb -> Svg
bombView bomb =
  let x = (bomb.position.x * blockSize) + (blockSize // 2)
      y = (bomb.position.y * blockSize) + (blockSize // 2)
  in g []
       [circle [cx (toString x)
               ,cy (toString y)
               ,r (toString (blockSize // 2))
               ,fill "black"]
               []
       ,ellipse [cx (toString x)
                ,cy (toString y)
                ,rx (toString (bomb.blastRadius * blockSize // 2))
                ,ry (toString (blockSize // 2))
                ,fill "red"]
                []
       ,ellipse [cx (toString x)
                ,cy (toString y)
                ,rx (toString (blockSize // 2))
                ,ry (toString (bomb.blastRadius * blockSize // 2))
                ,fill "red"]
                []]

playerView : Player -> Svg
playerView player =
  let x = (player.position.x * blockSize) + (blockSize // 2)
      y = (player.position.y * blockSize) + (blockSize // 2)
  in circle [cx (toString x)
            ,cy (toString y)
            ,r (toString (round ((toFloat blockSize) * 0.3)))
            ,stroke (playerColor player)
            ,strokeWidth (toString (round ((toFloat blockSize) * 0.2)))
            ,fill "transparent"]
            []

sceneView : Scene -> Svg
sceneView scene =
  svg [width (toString (blockSize * 11))
      ,height (toString (blockSize * 11))]
      ((List.map wallView scene.walls)
       ++
       (List.map bombView scene.bombs)
       ++
       (List.map playerView scene.players))
