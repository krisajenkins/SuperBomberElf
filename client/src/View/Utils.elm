module View.Utils exposing (..)

import String
import Svg exposing (Attribute)
import Svg.Attributes exposing (class)
import Tuple exposing (..)


classList : List ( String, Bool ) -> Attribute msg
classList =
    List.filter second >> List.map first >> String.join " " >> class
