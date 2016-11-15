module State exposing (..)

import Json.Decode as Json exposing (decodeString)
import Json.Encode as Encode
import Keyboard
import Time exposing (every, millisecond)
import Types exposing (..)
import WebSocket


init : ( Model, Cmd Msg )
init =
    ( { scene = Nothing
      , lastError = Nothing
      }
    , sendMessage (SetName "WLHN")
    )


websocketHost : String
websocketHost =
    "ws://localhost:8080"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketHost (decodeString decodeServerMessage >> MessageReceived)
        , every (200 * millisecond) Tick
        , Keyboard.ups decodeKey
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MessageReceived (Ok (SceneMessage scene)) ->
            ( { model | scene = Just scene }
            , Cmd.none
            )

        MessageReceived (Ok (HelpMessage _)) ->
            ( model, Cmd.none )

        MessageReceived (Err error) ->
            ( { model | lastError = Just error }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, sendMessage Look )

        PlayerMessage cmd ->
            ( model, sendMessage cmd )

        MessageSent ->
            ( model, Cmd.none )


sendMessage : PlayerCommand -> Cmd Msg
sendMessage =
    encodePlayerCommand
        >> Encode.encode 0
        >> WebSocket.send websocketHost
