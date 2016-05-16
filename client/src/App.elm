module App (..) where

import Effects exposing (Never)
import Html exposing (Html)
import Json.Decode as Json
import Keyboard
import Signal exposing (constant)
import StartApp exposing (..)
import State exposing (websocketMailbox)
import Task exposing (Task)
import Time
import Types exposing (..)
import View
import WebSocket as WS


------------------------------------------------------------
-- App Startup
------------------------------------------------------------


app : App Model
app =
  StartApp.start
    { init = State.init
    , view = View.root
    , update = State.update
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
