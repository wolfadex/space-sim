module Ui.Text exposing (defaultLabelLeft)

import Element exposing (..)
import Element.Extra
import Element.Input as Input
import Html.Attributes


defaultLabelLeft :
    List (Attribute msg)
    ->
        { id : String
        , onChange : String -> msg
        , text : String
        , labelAttributes : List (Attribute msg)
        , labelContent : Element msg
        }
    -> Element msg
defaultLabelLeft attributes config =
    Input.text
        (attributes ++ [ Element.Extra.id config.id ])
        { onChange = config.onChange
        , placeholder = Nothing
        , text = config.text
        , label =
            Input.labelLeft
                (config.labelAttributes
                    ++ [ htmlAttribute (Html.Attributes.for config.id) ]
                )
                config.labelContent
        }
