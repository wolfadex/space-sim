module Ui.Slider exposing (int)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Ui.Theme


int : { onChange : Int -> msg, label : Input.Label msg, min : Int, max : Int, value : Int, step : Maybe Int } -> Element msg
int options =
    Input.slider
        [ behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color Ui.Theme.darkGray
                , Border.rounded 2
                ]
                none
            )
        ]
        { onChange = \i -> options.onChange (floor i)
        , label = options.label
        , min = toFloat options.min
        , max = toFloat options.max
        , value = toFloat options.value
        , thumb = Input.defaultThumb
        , step = Maybe.map toFloat options.step
        }
