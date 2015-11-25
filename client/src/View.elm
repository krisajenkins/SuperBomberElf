module View where

import Html exposing (..)
import Types exposing (..)
import Signal exposing (Address)
import View.Scene exposing (sceneView)

root : Model -> Address Action -> Html
root model address =
  div []
      [h1 []
          [Html.text "Super Bomber Elf"]
      ,case model.scene of
        Just (Ok scene) -> sceneView scene
        Just (Err e) -> Html.text ("ERROR: " ++ e)
        Nothing -> i [] [Html.text "Waiting for data..."]]

debuggingView : Model -> Html
debuggingView model =
  case model.scene of
   Just (Ok scene) -> div []
                          [div [] [code [] [text (toString scene.bombs)]]
                          ,div [] [code [] [text (toString scene.players)]]]
   _ -> span [] []
