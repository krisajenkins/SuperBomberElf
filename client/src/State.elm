module State where

import Effects exposing (..)
import Signal exposing (Signal,Mailbox)
import Types exposing (..)
import Json.Encode as Encode

init : (Model, Effects Action)
init =
  ({scene = Nothing
  ,lastError = Nothing}
  ,sendMessage (SetName "WLHN"))

update : Action -> Model -> Model
update action model =
  case action of
    MessageReceived (Ok (SceneMessage scene)) -> {model | scene = Just scene}
    MessageReceived (Ok (HelpMessage _)) -> model
    MessageReceived (Err error) -> {model | lastError = Just error}
    NoOp -> model
    Tick _ -> model
    PlayerMessage _ -> model
    MessageSent -> model

effect : Action -> Model -> Effects Action
effect action model =
  case action of
    MessageReceived _ -> none
    NoOp -> none
    Tick _ -> sendMessage Look
    PlayerMessage msg -> sendMessage msg
    MessageSent -> none

sendMessage : PlayerCommand -> Effects Action
sendMessage msg =
  Encode.encode 0 (encodePlayerCommand msg)
    |> Signal.send websocketMailbox.address
    |> Effects.task
    |> Effects.map (always MessageSent)

websocketMailbox : Mailbox String
websocketMailbox = Signal.mailbox ""
