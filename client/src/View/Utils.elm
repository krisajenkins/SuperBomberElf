module View.Utils where

import Svg exposing (Attribute)
import Svg.Attributes exposing (class)
import String

classList : List (String,Bool) -> Attribute
classList = List.filter snd >> List.map fst >> String.join " " >> class
