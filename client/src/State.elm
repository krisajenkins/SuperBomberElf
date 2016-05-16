module State (..) where

import Effects exposing (..)
import Signal exposing (Signal, Mailbox)
import Types exposing (..)
import Json.Encode as Encode


init : ( Model, Effects Action )
init =
  ( { scene = Nothing
    , lastError = Nothing
    }
  , sendMessage (SetName "WLHN")
  )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    MessageReceived (Ok (SceneMessage scene)) ->
      ( { model | scene = Just scene }
      , none
      )

    MessageReceived (Ok (HelpMessage _)) ->
      ( model, none )

    MessageReceived (Err error) ->
      ( { model | lastError = Just error }
      , none
      )

    NoOp ->
      ( model, none )

    Tick _ ->
      ( model, sendMessage Look )

    PlayerMessage cmd ->
      ( model, sendMessage cmd )

    MessageSent ->
      ( model, none )


sendMessage : PlayerCommand -> Effects Action
sendMessage msg =
  Encode.encode 0 (encodePlayerCommand msg)
    |> Signal.send websocketMailbox.address
    |> Effects.task
    |> Effects.map (always MessageSent)


websocketMailbox : Mailbox String
websocketMailbox =
  Signal.mailbox ""
