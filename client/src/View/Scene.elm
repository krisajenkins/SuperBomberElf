module View.Scene where

import Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

playerView : Player -> Svg
playerView player =
  circle [cx (player.position.x * 20 |> toString)
         ,cy (player.position.y * 20 |> toString)
         ,r "10px"]
         []

sceneView : Scene -> Svg
sceneView scene =
  svg [width "400px"
      ,height "300px"]
      (List.map playerView scene.players)
