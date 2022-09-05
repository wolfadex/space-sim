module Element.Extra exposing (id)

import Element exposing (..)
import Html.Attributes


id : String -> Attribute msg
id str =
    htmlAttribute (Html.Attributes.id str)
