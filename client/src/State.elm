module State where

import Effects exposing (..)
import Signal exposing (Signal,Mailbox)
import Types exposing (..)
import Json.Encode as Encode

init : (Model, Effects Action)
init =
  ({scene = Nothing}
  ,none)

update : Action -> Model -> Model
update action model =
  case action of
    NewScene scene -> {model | scene = Just scene}
    Message _ -> model
    MessageSent -> model

effect : Action -> Model -> Effects Action
effect action model =
  case action of
    NewScene _ -> none
    Message msg -> Encode.encode 0 (Encode.object [("message", (encodePlayerCommand msg))])
                   |> Signal.send websocketMailbox.address
                   |> Effects.task
                   |> Effects.map (always MessageSent)
    MessageSent -> none

websocketMailbox : Mailbox String
websocketMailbox = Signal.mailbox ""
