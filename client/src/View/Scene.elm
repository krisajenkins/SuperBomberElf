module View.Scene where

import Types exposing (..)
import Svg exposing (..)
import String
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
       ,opacity (if wall.alive
                 then "1"
                 else "0.5")
       ,stroke "white"]
       []

playerColor : Player -> String
playerColor player =
  player.name
  |> Maybe.map (String.slice 0 6)
  |> Maybe.map ((++) "#")
  |> Maybe.withDefault "black"

bombView : Bomb -> Svg
bombView bomb =
  let x = (bomb.position.x * blockSize) + (blockSize // 2)
      y = (bomb.position.y * blockSize) + (blockSize // 2)
      blastLine toX toY =
        line [x1 (toString x)
             ,y1 (toString y)
             ,x2 (toString toX)
             ,y2 (toString toY)
             ,stroke "red"
             ,strokeWidth (toString (blockSize // 2))
             ,strokeLinecap "round"]
             []
  in g []
       (case bomb.blast of
         Nothing -> []
         Just blast -> [blastLine x (y - (blast.north * blockSize))
                       ,blastLine x (y + (blast.south * blockSize))
                       ,blastLine (x - (blast.west * blockSize)) y
                       ,blastLine (x + (blast.east * blockSize)) y]
       ++
       [circle [cx (toString x)
               ,cy (toString y)
               ,r (toString (blockSize // 2))
               ,fill "black"]
               []])

playerView : Player -> Svg
playerView player =
  let x = (player.position.x * blockSize) + (blockSize // 2)
      y = (player.position.y * blockSize) + (blockSize // 2)
  in circle [cx (toString x)
            ,cy (toString y)
            ,r (toString (round ((toFloat blockSize) * 0.3)))
            ,stroke (playerColor player)
            ,strokeWidth (toString (round ((toFloat blockSize) * 0.2)))
            ,opacity (if player.alive
                      then "1"
                      else "0.5")
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
