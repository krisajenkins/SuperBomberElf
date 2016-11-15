module App exposing (..)

import Html exposing (..)
import State
import Types exposing (Model, Msg)
import View


------------------------------------------------------------
-- App Startup
------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.program
        { init = State.init
        , view = View.root
        , update = State.update
        , subscriptions = State.subscriptions
        }
