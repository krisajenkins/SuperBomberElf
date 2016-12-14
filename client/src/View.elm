module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Types exposing (..)
import View.Scene exposing (sceneView, playerView)


link : List (Attribute msg) -> List (Html msg) -> Html msg
link =
    node "link"


root : Model -> Html Msg
root model =
    div
        [ style
            [ ( "font-family", "Amarante, sans-serif" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ link
            [ href "//fonts.googleapis.com/css?family=Amarante&subset=latin"
            , rel "stylesheet"
            , type_ "text/css"
            ]
            []
        , link
            [ href "main.css"
            , rel "stylesheet"
            , type_ "text/css"
            ]
            []
        , h1 [ style [ ( "font-size", "3rem" ) ] ]
            [ text "Super Bomber Elf" ]
        , case model.scene of
            Just scene ->
                sceneLoadedView scene

            Nothing ->
                i [] [ text "Waiting for data..." ]
        ]


sceneLoadedView : Scene -> Html Msg
sceneLoadedView scene =
    div [ style [ ( "display", "flex" ) ] ]
        [ div [ style [ ( "width", "40vw" ) ] ] [ playerGuideView scene ]
        , div [ style [ ( "width", "40vw" ) ] ] [ sceneView scene ]
        ]


playerSort : Player -> Player -> Order
playerSort a b =
    case ( a.name, b.name ) of
        ( Nothing, Nothing ) ->
            compare a.score b.score

        ( Just _, Nothing ) ->
            LT

        ( Nothing, Just _ ) ->
            GT

        ( Just aName, Just bName ) ->
            compare a.score b.score


playerGuideView : Scene -> Html Msg
playerGuideView scene =
    div []
        (List.map playerBadgeView
            (List.sortWith playerSort scene.players)
        )


playerBadgeView : Player -> Html Msg
playerBadgeView player =
    div
        [ style
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            ]
        ]
        [ span
            [ style
                [ ( "font-size", "1.3rem" )
                , ( "margin", "0.5rem" )
                ]
            ]
            [ text (toString player.score) ]
        , Svg.svg
            [ Svg.Attributes.width "40"
            , Svg.Attributes.height "40"
            , Svg.Attributes.viewBox "0 0 50 50"
            , Svg.Attributes.preserveAspectRatio "xMaxYMax"
            ]
            [ playerView { player | position = Position 0 0 } ]
        , span
            [ style
                [ ( "font-size", "1.3rem" )
                , ( "margin", "0.5rem" )
                ]
            ]
            [ text (Maybe.withDefault player.id player.name) ]
        ]


debuggingView : Model -> Html Msg
debuggingView model =
    case model.scene of
        Just scene ->
            div []
                [ div [] [ code [] [ text (toString scene.bombs) ] ]
                , div [] [ code [] [ text (toString scene.players) ] ]
                ]

        _ ->
            span [] []
