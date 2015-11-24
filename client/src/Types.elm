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
  ,position : Position}

type alias Player =
  {name : Maybe String
  ,position : Position}

type alias Bomb =
  {position : Position
  ,blastRadius : Int}

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

type PlayerCommand
  = NoCommand
  | DropBomb
  | North
  | South
  | East
  | West

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
  object2 Wall
    ("type" := decodeWallType)
    ("position" := decodePosition)

decodePlayer : Decoder Player
decodePlayer =
  object2 Player
    ("name" := maybe string)
    ("position" := decodePosition)

decodeBomb : Decoder Bomb
decodeBomb =
  object2 Bomb
    ("position" := decodePosition)
    ("blastRadius" := int)

decodeScene : Decoder Scene
decodeScene =
  object3 Scene
    ("walls" := list decodeWall)
    ("players" := list decodePlayer)
    ("bombs" := list decodeBomb)

encodePlayerCommand : PlayerCommand -> Value
encodePlayerCommand command =
   Encode.string (toString command)

direction : { x : Int, y : Int } -> PlayerCommand
direction d =
  case (d.x,d.y) of
    (-1,0) -> West
    (1,0) -> East
    (0,-1) -> South
    (0,1) -> North
    _ -> NoCommand
