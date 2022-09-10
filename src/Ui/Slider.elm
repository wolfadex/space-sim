module Ui.Slider exposing (float, int)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Ui.Theme


int :
    List (Attribute msg)
    ->
        { onChange : Int -> msg
        , label :
            Input.Label msg
        , min : Int
        , max : Int
        , value : Int
        , step : Maybe Int
        }
    -> Element msg
int attributes options =
    common attributes
        { onChange = \i -> options.onChange (floor i)
        , label = options.label
        , min = toFloat options.min
        , max = toFloat options.max
        , value = toFloat options.value
        , thumb = Input.defaultThumb
        , step = Maybe.map toFloat options.step
        }


float :
    List (Attribute msg)
    ->
        { onChange : Float -> msg
        , label :
            Input.Label msg
        , min : Float
        , max : Float
        , value : Float
        , step : Maybe Float
        }
    -> Element msg
float attributes options =
    common attributes
        { onChange = options.onChange
        , label = options.label
        , min = options.min
        , max = options.max
        , value = options.value
        , thumb = Input.defaultThumb
        , step = options.step
        }


common :
    List (Attribute msg)
    ->
        { onChange : Float -> msg
        , label : Input.Label msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Input.Thumb
        , step : Maybe Float
        }
    -> Element msg
common attributes =
    Input.slider
        (behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color Ui.Theme.nearlyWhite
                , Border.rounded 2
                ]
                none
            )
            :: attributes
        )
