module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg.Attributes
import Svg
import Types exposing (..)
import Signal exposing (Address)
import View.Scene exposing (sceneView,playerView)

root : Model -> Address Action -> Html
root model address =
  div [style [("font-family", "Amarante, sans-serif")
             ,("display", "flex")
             ,("flex-direction", "column")
             ,("align-items", "center")]]
      [node "link" [href "//fonts.googleapis.com/css?family=Amarante&subset=latin"
            ,rel "stylesheet"
            ,type' "text/css"]
            []
      ,h1 [style [("font-size", "3rem")]]
          [Html.text "Super Bomber Elf"]
      ,case model.scene of
        Just (Ok scene) -> sceneLoadedView scene
        Just (Err e) -> Html.text ("ERROR: " ++ e)
        Nothing -> i [] [Html.text "Waiting for data..."]]

sceneLoadedView : Scene -> Html
sceneLoadedView scene =
  div [style [("display", "flex")]]
      [sceneView scene
      ,playerGuideView scene]

playerGuideView : Scene -> Html
playerGuideView scene =
  div []
      (List.map playerBadgeView scene.players)

playerBadgeView : Player -> Html
playerBadgeView player =
  div [style [("display", "flex")
             ,("align-items", "center")]]
      [span [style [("font-size", "1.6rem")
                   ,("margin", "1.2rem")]]
            [text (toString player.score)]
      ,Svg.svg [Svg.Attributes.width "60"
               ,Svg.Attributes.height "60"
               ,Svg.Attributes.viewBox "0 0 50 50"
               ,Svg.Attributes.preserveAspectRatio "xMaxYMax"]
           [playerView {player | position = Position 0 0}]
      ,span [style [("font-size", "1.6rem")
                   ,("margin", "1.2rem")]]
            [text (Maybe.withDefault player.id player.name)]]

debuggingView : Model -> Html
debuggingView model =
  case model.scene of
   Just (Ok scene) -> div []
                          [div [] [code [] [text (toString scene.bombs)]]
                          ,div [] [code [] [text (toString scene.players)]]]
   _ -> span [] []
