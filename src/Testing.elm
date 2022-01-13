module Testing exposing (testElementAttibute, testHtmlAttribute)

import Element exposing (..)
import Html
import Html.Attributes


testHtmlAttribute : String -> Html.Attribute msg
testHtmlAttribute =
    Html.Attributes.attribute "data-test"


testElementAttibute : String -> Attribute msg
testElementAttibute =
    testHtmlAttribute >> htmlAttribute
