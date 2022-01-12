module Ui.Button exposing (toggle)

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
                rgb 0.4 0.9 0.7

            else
                rgb 0 0 0
        , Font.color <|
            if config.enabled then
                rgb 0.2 0.2 0.2

            else
                rgb 0.9 0.9 0.9
        , Border.color (rgb 0.2 0.2 0.2)
        ]
        { onPress = config.onPress
        , label = config.label
        }
