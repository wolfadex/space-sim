module Element.Extra exposing (id)

import Element exposing (..)
import Html.Attributes


id : String -> Attribute msg
id =
    Html.Attributes.id >> htmlAttribute
