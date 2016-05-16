module View.Scene (..) where

import Types exposing (..)
import Svg exposing (..)
import String
import View.Utils exposing (classList)
import Svg.Attributes exposing (..)
import View.Bomb exposing (bombIcon)
import View.Player exposing (playerIcon)


wallColor : WallType -> String
wallColor t =
  case t of
    Strong ->
      "teal"

    Weak ->
      "lightsalmon"


blockSize : Int
blockSize =
  35


wallView : Wall -> Svg
wallView wall =
  rect
    [ x (toString (wall.position.x * blockSize))
    , y (toString (wall.position.y * blockSize))
    , width (toString blockSize)
    , height (toString blockSize)
    , fill (wallColor wall.wallType)
    , classList
        [ ( "wall", True )
        , ( "alive", wall.alive )
        ]
    , stroke "white"
    ]
    []


playerColor : Player -> String
playerColor player =
  player.id
    |> (String.slice 0 6)
    |> ((++) "#")


bombView : Bomb -> Svg
bombView bomb =
  let
    x =
      (bomb.position.x * blockSize)

    y =
      (bomb.position.y * blockSize)

    blastLine toX toY =
      line
        [ x1 (toString (x + (blockSize // 2)))
        , y1 (toString (y + (blockSize // 2)))
        , x2 (toString (toX + (blockSize // 2)))
        , y2 (toString (toY + (blockSize // 2)))
        , stroke "red"
        , strokeWidth (toString (blockSize // 2))
        , strokeLinecap "round"
        ]
        []
  in
    g
      []
      ((case bomb.blast of
          Nothing ->
            []

          Just blast ->
            [ blastLine x (y - (blast.north * blockSize))
            , blastLine x (y + (blast.south * blockSize))
            , blastLine (x - (blast.west * blockSize)) y
            , blastLine (x + (blast.east * blockSize)) y
            ]
       )
        ++ [ bombIcon
              (toString x)
              (toString y)
              (toString blockSize)
              (toString blockSize)
           ]
      )


playerView : Player -> Svg
playerView player =
  let
    x =
      (player.position.x * blockSize)

    y =
      (player.position.y * blockSize)
  in
    playerIcon
      player.alive
      (playerColor player)
      (toString x)
      (toString y)
      (toString blockSize)
      (toString blockSize)


sceneView : Scene -> Svg
sceneView scene =
  svg
    [ width (toString (blockSize * 11))
    , height (toString (blockSize * 11))
    ]
    ((List.map wallView scene.walls)
      ++ (List.map bombView scene.bombs)
      ++ (List.map playerView scene.players)
    )
