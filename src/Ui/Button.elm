module Ui.Button exposing
    ( default
    , primary
    , toggle
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


toggle : { onPress : Maybe msg, label : Element msg, enabled : Bool } -> Element msg
toggle config =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color <|
            if config.enabled then
                green

            else
                darkGray
        , Font.color <|
            if config.enabled then
                darkGray

            else
                nearlyWhite
        , Border.color darkGray
        ]
        { onPress = config.onPress
        , label = config.label
        }


primary : { onPress : Maybe msg, label : Element msg } -> Element msg
primary =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color green
        , Font.color darkGray
        , Border.color darkGray
        ]


default : { onPress : Maybe msg, label : Element msg } -> Element msg
default =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color darkGray
        , Font.color nearlyWhite
        , Border.color darkGray
        ]


nearlyWhite : Color
nearlyWhite =
    rgb 0.9 0.9 0.9


darkGray : Color
darkGray =
    rgb 0.2 0.2 0.2


green : Color
green =
    rgb 0.4 0.9 0.7
