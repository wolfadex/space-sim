module Ui.Button exposing
    ( default
    , inspect
    , primary
    , toggle
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Ui.Theme


toggle : { onPress : Maybe msg, label : Element msg, enabled : Bool } -> Element msg
toggle config =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color <|
            if config.enabled then
                Ui.Theme.green

            else
                Ui.Theme.darkGray
        , Font.color <|
            if config.enabled then
                Ui.Theme.darkGray

            else
                Ui.Theme.nearlyWhite
        , Border.color Ui.Theme.darkGray
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
        , Background.color Ui.Theme.green
        , Font.color Ui.Theme.darkGray
        , Border.color Ui.Theme.darkGray
        ]


default : { onPress : Maybe msg, label : Element msg } -> Element msg
default =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color Ui.Theme.darkGray
        , Font.color Ui.Theme.nearlyWhite
        , Border.color Ui.Theme.darkGray
        ]


inspect : Maybe msg -> Element msg
inspect onPress =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color Ui.Theme.darkGray
        , Font.color Ui.Theme.nearlyWhite
        , Border.color Ui.Theme.darkGray
        ]
        { label = text "👁"
        , onPress = onPress
        }
