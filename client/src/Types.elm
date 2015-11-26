module Types where

import Json.Decode exposing (..)
import Json.Encode as Encode

type alias Position =
  {x : Int
  ,y : Int}

type WallType
  = Weak
  | Strong

type alias Wall =
  {wallType : WallType
  ,position : Position
  ,alive : Bool}

type alias Player =
  {id : String
  ,name : Maybe String
  ,position : Position
  ,alive : Bool
  ,score : Int}

type alias Blast =
  {north : Int
  ,south : Int
  ,west : Int
  ,east : Int}

type alias Bomb =
  {position : Position
  ,blast : Maybe Blast}

type alias Scene =
  {walls : List Wall
  ,players : List Player
  ,bombs : List Bomb}

type alias Model =
  {scene : Maybe (Result String Scene)}

type Action
  = MessageSent
  | NewScene (Result String Scene)
  | Message PlayerCommand
  | NoOp

type Direction
  = North
  | South
  | West
  | East

type PlayerCommand
  = DropBomb
  | SetName String
  | Move Direction

decodePosition : Decoder Position
decodePosition =
  object2 Position
    ("x" := int)
    ("y" := int)

decodeWallType : Decoder WallType
decodeWallType =
  string `andThen`
    (\s -> case s of
             "Weak" -> succeed Weak
             "Strong" -> succeed Strong
             _ -> fail ("Unknown wall type: " ++ s))

decodeWall : Decoder Wall
decodeWall =
  object3 Wall
    ("type" := decodeWallType)
    ("position" := decodePosition)
    ("alive" := bool)

decodePlayer : Decoder Player
decodePlayer =
  object5 Player
    ("id" := string)
    ("name" := maybe string)
    ("position" := decodePosition)
    ("alive" := bool)
    ("score" := int)

decodeBlast : Decoder Blast
decodeBlast =
  object4 Blast
    ("North" := int)
    ("South" := int)
    ("West" := int)
    ("East" := int)

decodeBomb : Decoder Bomb
decodeBomb =
  object2 Bomb
    ("position" := decodePosition)
    ("blast" := maybe decodeBlast)

decodeScene : Decoder Scene
decodeScene =
  object3 Scene
    ("walls" := list decodeWall)
    ("players" := list decodePlayer)
    ("bombs" := list decodeBomb)

encodePlayerCommand : PlayerCommand -> Value
encodePlayerCommand command =
  Encode.object
    [("command", (case command of
                    DropBomb -> Encode.string "DropBomb"
                    SetName name -> Encode.list <| List.map Encode.string ["SetName",name]
                    Move d -> Encode.string <| "Move" ++ toString d))]

directionFor : { x : Int, y : Int } -> Maybe Direction
directionFor d =
  case (d.x,d.y) of
    (-1,0) -> Just West
    (1,0) -> Just East
    (0,-1) -> Just South
    (0,1) -> Just North
    _ -> Nothing

messageFor : { x : Int, y : Int } -> Maybe Action
messageFor d =
  Maybe.map (Message << Move) (directionFor d)
