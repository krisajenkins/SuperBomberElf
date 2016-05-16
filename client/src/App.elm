module App (..) where

import StartApp exposing (..)
import State exposing (websocketMailbox)
import Time
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import View
import Keyboard
import Types exposing (..)
import Signal exposing (constant)
import Json.Decode as Json
import WebSocket as WS


------------------------------------------------------------
-- App Startup
------------------------------------------------------------


app : App Model
app =
  StartApp.start
    { init = State.init
    , view = flip View.root
    , update =
        \action model ->
          let
            newModel =
              State.update action model

            newEffects =
              State.effect action newModel
          in
            ( newModel, newEffects )
    , inputs =
        [ Signal.map
            (MessageReceived << Json.decodeString decodeServerMessage)
            (WS.connect
              "ws://localhost:8080"
              websocketMailbox.signal
            )
        , Signal.filterMap messageFor NoOp Keyboard.arrows
        , Signal.filterMap messageFor NoOp Keyboard.wasd
        , Signal.map Tick (Time.fps 5)
        , Signal.map
            (\v ->
              if v then
                PlayerMessage DropBomb
              else
                NoOp
            )
            Keyboard.space
        ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
