module Ui.Text exposing (default)

import Element exposing (..)
import Element.Extra
import Element.Input as Input exposing (Label)
import Html.Attributes


default :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , label : Label msg
        }
    -> Element msg
default attributes config =
    Input.text
        attributes
        { onChange = config.onChange
        , placeholder = Nothing
        , text = config.text
        , label = config.label
        }
