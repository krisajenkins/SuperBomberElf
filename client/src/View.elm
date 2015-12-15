module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg.Attributes
import Svg
import Types exposing (..)
import Signal exposing (Address)
import View.Scene exposing (sceneView,playerView)

link : List Attribute -> List Html -> Html
link = node "link"

root : Model -> Address Action -> Html
root model address =
  div [style [("font-family", "Amarante, sans-serif")
             ,("display", "flex")
             ,("flex-direction", "column")
             ,("align-items", "center")]]
      [link [href "//fonts.googleapis.com/css?family=Amarante&subset=latin"
            ,rel "stylesheet"
            ,type' "text/css"]
            []
      ,link [href "style.css"
            ,rel "stylesheet"
            ,type' "text/css"]
            []
      ,h1 [style [("font-size", "3rem")]]
          [text "Super Bomber Elf"]
      ,case model.scene of
        Just scene -> sceneLoadedView scene
        Nothing -> i [] [text "Waiting for data..."]]

sceneLoadedView : Scene -> Html
sceneLoadedView scene =
  div [style [("display", "flex")]]
      [div [style [("width", "40vw")]] [playerGuideView scene]
      ,div [style [("width", "40vw")]] [sceneView scene]]

playerGuideView : Scene -> Html
playerGuideView scene =
  div []
      (List.map playerBadgeView scene.players)

playerBadgeView : Player -> Html
playerBadgeView player =
  div [style [("display", "flex")
             ,("align-items", "center")]]
      [span [style [("font-size", "1.3rem")
                   ,("margin", "0.5rem")]]
            [text (toString player.score)]
      ,Svg.svg [Svg.Attributes.width "40"
               ,Svg.Attributes.height "40"
               ,Svg.Attributes.viewBox "0 0 50 50"
               ,Svg.Attributes.preserveAspectRatio "xMaxYMax"]
           [playerView {player | position = Position 0 0}]
      ,span [style [("font-size", "1.3rem")
                   ,("margin", "0.5rem")]]
            [text (Maybe.withDefault player.id player.name)]]

debuggingView : Model -> Html
debuggingView model =
  case model.scene of
   Just scene -> div []
                     [div [] [code [] [text (toString scene.bombs)]]
                     ,div [] [code [] [text (toString scene.players)]]]
   _ -> span [] []
