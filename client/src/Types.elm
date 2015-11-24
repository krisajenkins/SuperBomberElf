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

type alias Scene =
  {walls : List Wall
  ,players : List Player}

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

decodeScene : Decoder Scene
decodeScene =
  object2 Scene
    ("walls" := list decodeWall)
    ("players" := list decodePlayer)

encodePlayerCommand : PlayerCommand -> Value
encodePlayerCommand command =
   Encode.string (toString command)
