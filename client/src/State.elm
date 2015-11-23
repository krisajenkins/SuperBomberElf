module State where

import Effects exposing (..)
import Types exposing (..)

init : (Model, Effects Action)
init =
  ({scene = Nothing}
  ,none)

update : Action -> Model -> Model
update action model =
  case action of
    NewScene scene -> {model | scene = Just scene}

effect : Action -> Model -> Effects Action
effect action model = none
