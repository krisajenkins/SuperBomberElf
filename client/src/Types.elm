module Types where

import Json.Decode exposing (..)

type alias Position =
  {x : Int
  ,y : Int}

type alias Player =
  {name : Maybe String
  ,position : Position}

type alias Scene =
  {players : List Player}

type alias Model =
  {scene : Maybe (Result String Scene)}

type Action
  = NewScene (Result String Scene)

decodePosition : Decoder Position
decodePosition =
  object2 Position
    ("x" := int)
    ("y" := int)

decodePlayer : Decoder Player
decodePlayer =
  object2 Player
    ("name" := maybe string)
    ("position" := decodePosition)

decodeScene : Decoder Scene
decodeScene =
  object1 Scene
    ("players" := list decodePlayer)
