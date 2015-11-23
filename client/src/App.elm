module App where

import StartApp exposing (..)
import State
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import View
import Types exposing (..)
import Signal exposing (constant)
import Json.Decode as Json
import WebSocket as WS

------------------------------------------------------------
-- App Startup
------------------------------------------------------------

app : App Model
app = StartApp.start {init = State.init
                     ,view = flip View.root
                     ,update = \action model -> let newModel = State.update action model
                                                    newEffects = State.effect action newModel
                                                in (newModel, newEffects)
                     ,inputs = [Signal.map (NewScene << Json.decodeString decodeScene)
                                           (WS.connect "ws://localhost:8080" (constant ""))]}

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
