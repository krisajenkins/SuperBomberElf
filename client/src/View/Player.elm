module View.Player exposing (..)

import Svg exposing (svg, g, path, circle, Attribute, Svg)
import View.Utils exposing (classList)
import Svg.Attributes exposing (d, x, y, cx, cy, r, preserveAspectRatio, width, height, viewBox, fill, fillRule, stroke, strokeWidth, style, class)


cls124 : String
cls124 =
    """
        stroke: #272525;
        stroke-width: 3px;
      """


cls234 : String
cls234 =
    """
        fill-rule: evenodd;
      """


cls1 : String
cls1 =
    """
        fill: #ff33e0;
      """ ++ cls124


cls3 : String
cls3 =
    """
        fill: #ffcd9e;
      """ ++ cls234


cls4 : String
cls4 =
    """
        fill: #272525;
      """ ++ cls234


playerIcon : Bool -> String -> String -> String -> String -> String -> Svg msg
playerIcon alive color x1 y1 w h =
    let
        cls2 =
            "fill: " ++ color ++ ";" ++ cls234
    in
        svg
            [ preserveAspectRatio "xMidYMid"
            , width w
            , x x1
            , y y1
            , classList
                [ ( "player", True )
                , ( "alive", alive )
                ]
            , height h
            , viewBox "0 0 112.625 102.375"
            ]
            [ g []
                ([ circle [ cx "15", cy "15", r "13.5", style cls1 ] []
                 , path [ d "M26.500,18.500 L73.500,18.500 C84.546,18.500 93.500,27.454 93.500,38.500 L93.500,75.500 C93.500,81.023 89.023,85.500 83.500,85.500 L16.500,85.500 C10.977,85.500 6.500,81.023 6.500,75.500 L6.500,38.500 C6.500,27.454 15.454,18.500 26.500,18.500 Z", style cls2 ] []
                 , path [ d "M29.000,27.000 L68.000,27.000 C75.732,27.000 82.000,33.268 82.000,41.000 L82.000,70.000 C82.000,72.209 80.209,74.000 78.000,74.000 L19.000,74.000 C16.791,74.000 15.000,72.209 15.000,70.000 L15.000,41.000 C15.000,33.268 21.268,27.000 29.000,27.000 Z", style cls3 ] []
                 , path [ d "M33.500,34.500 C35.157,34.500 36.500,35.843 36.500,37.500 L36.500,65.500 C36.500,67.157 35.157,68.500 33.500,68.500 C31.843,68.500 30.500,67.157 30.500,65.500 L30.500,37.500 C30.500,35.843 31.843,34.500 33.500,34.500 Z", style cls4 ] []
                 , path [ d "M65.500,34.500 C67.157,34.500 68.500,35.843 68.500,37.500 L68.500,65.500 C68.500,67.157 67.157,68.500 65.500,68.500 C63.843,68.500 62.500,67.157 62.500,65.500 L62.500,37.500 C62.500,35.843 63.843,34.500 65.500,34.500 Z", style cls4 ] []
                 ]
                    ++ (if alive then
                            []
                        else
                            [ path [ d "M18.464,24.000 L16.000,32.000 L39.000,37.000 L39.000,34.600 L18.464,24.000 Z", style cls4 ] []
                            , path [ d "M77.536,24.000 L80.000,32.000 L57.000,37.000 L57.000,34.600 L77.536,24.000 Z", style cls4 ] []
                            ]
                       )
                )
            ]
