module View where

import Html exposing (..)
import Types exposing (..)
import Signal exposing (Address)

playerView : Player -> Html
playerView player =
  div []
      [code []
            [Html.text (Maybe.withDefault "UNNAMED" player.name)]]

sceneView : Scene -> Address Action -> Html
sceneView scene address =
  div []
      [code []
            (List.map playerView scene.players)]

root : Model -> Address Action -> Html
root model address =
  div []
      [h1 []
          [Html.text "Bomberman"]
      ,div []
           [code []
                 [Html.text <| toString model]]
      ,case model.scene of
        Just (Ok scene) -> sceneView scene address
        Just (Err e) -> Html.text ("ERROR: " ++ e)
        Nothing -> Html.text "Waiting for data..."]
