module Ui.Text exposing (default)

import Element exposing (..)
import Element.Extra
import Element.Input as Input exposing (Label)
import Html.Attributes


default :
    List (Attribute msg)
    ->
        { id : String
        , onChange : String -> msg
        , text : String
        , label : Label msg
        }
    -> Element msg
default attributes config =
    Input.text
        (attributes
            ++ [ Element.Extra.id config.id
               , htmlAttribute (Html.Attributes.attribute "aria-label" "")
               ]
        )
        { onChange = config.onChange
        , placeholder = Nothing
        , text = config.text
        , label = config.label
        }
