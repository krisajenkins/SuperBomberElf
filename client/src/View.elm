module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Signal exposing (Address)
import View.Scene exposing (sceneView)

root : Model -> Address Action -> Html
root model address =
  div []
      [h1 []
          [Html.text "Super Bomber Elf"]
      ,joystick address
      ,case model.scene of
        Just (Ok scene) -> sceneView scene
        Just (Err e) -> Html.text ("ERROR: " ++ e)
        Nothing -> i [] [Html.text "Waiting for data..."]]

joystick : Address Action -> Html
joystick address =
  div [class "btn-group"]
      [button [class "btn", onClick address (Message West)] [text "West"]
      ,button [class "btn", onClick address (Message North)] [text "North"]
      ,button [class "btn", onClick address (Message South)] [text "South"]
      ,button [class "btn", onClick address (Message East)] [text "East"]
      ,button [class "btn", onClick address (Message DropBomb)] [text "Bomb"]]
