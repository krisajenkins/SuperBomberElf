module Types exposing (..)

import Char
import Json.Decode exposing (..)
import Json.Encode as Encode
import Keyboard exposing (KeyCode)


type alias Position =
    { x : Int
    , y : Int
    }


type WallType
    = Weak
    | Strong


type alias Wall =
    { wallType : WallType
    , position : Position
    , alive : Bool
    }


type alias Player =
    { id : String
    , name : Maybe String
    , position : Position
    , alive : Bool
    , score : Int
    }


type alias Blast =
    { north : Int
    , south : Int
    , west : Int
    , east : Int
    }


type alias Bomb =
    { position : Position
    , blast : Maybe Blast
    }


type alias Scene =
    { walls : List Wall
    , players : List Player
    , bombs : List Bomb
    }


type alias Model =
    { scene : Maybe Scene
    , lastError : Maybe String
    }


type alias Help =
    { validCommands : List String }


type ServerMessage
    = SceneMessage Scene
    | HelpMessage Help


type Msg
    = MessageSent
    | MessageReceived (Result String ServerMessage)
    | PlayerMessage PlayerCommand
    | Tick Float
    | NoOp


type Direction
    = North
    | South
    | West
    | East


type PlayerCommand
    = DropBomb
    | Look
    | SetName String
    | Move Direction


decodePosition : Decoder Position
decodePosition =
    map2 Position
        (field "x" int)
        (field "y" int)


decodeWallType : Decoder WallType
decodeWallType =
    string
        |> andThen
            (\s ->
                case s of
                    "Weak" ->
                        succeed Weak

                    "Strong" ->
                        succeed Strong

                    _ ->
                        fail ("Unknown wall type: " ++ s)
            )


decodeWall : Decoder Wall
decodeWall =
    map3 Wall
        (field "type" decodeWallType)
        (field "position" decodePosition)
        (field "alive" bool)


decodePlayer : Decoder Player
decodePlayer =
    map5 Player
        (field "id" string)
        (field "name" (maybe string))
        (field "position" decodePosition)
        (field "alive" bool)
        (field "score" int)


decodeBlast : Decoder Blast
decodeBlast =
    map4 Blast
        (field "North" int)
        (field "South" int)
        (field "West" int)
        (field "East" int)


decodeBomb : Decoder Bomb
decodeBomb =
    map2 Bomb
        (field "position" decodePosition)
        (field "blast" (maybe decodeBlast))


decodeScene : Decoder Scene
decodeScene =
    map3 Scene
        (field "walls" (list decodeWall))
        (field "players" (list decodePlayer))
        (field "bombs" (list decodeBomb))


decodeHelp : Decoder Help
decodeHelp =
    map Help
        (field "validCommands" (list string))


decodeServerMessage : Decoder ServerMessage
decodeServerMessage =
    oneOf
        [ map SceneMessage decodeScene
        , map HelpMessage decodeHelp
        ]


encodePlayerCommand : PlayerCommand -> Value
encodePlayerCommand command =
    Encode.object
        [ ( "command"
          , (case command of
                DropBomb ->
                    Encode.string "DropBomb"

                Look ->
                    Encode.string "Look"

                SetName name ->
                    Encode.list <| List.map Encode.string [ "SetName", name ]

                Move d ->
                    Encode.string <| "Move" ++ toString d
            )
          )
        ]


decodeKey : KeyCode -> Msg
decodeKey keyCode =
    case Char.fromCode keyCode |> Char.toLower of
        ' ' ->
            PlayerMessage DropBomb

        'w' ->
            PlayerMessage (Move North)

        'a' ->
            PlayerMessage (Move West)

        's' ->
            PlayerMessage (Move South)

        'd' ->
            PlayerMessage (Move East)

        _ ->
            NoOp
