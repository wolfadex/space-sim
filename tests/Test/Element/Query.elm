module Test.Element.Query exposing (fromElement, has)

import Element exposing (Element)
import Expect exposing (Expectation)
import Test.Html.Query exposing (Single)
import Test.Html.Selector exposing (Selector)


fromElement : Element msg -> Single msg
fromElement element =
    Test.Html.Query.fromHtml (Element.layout [] element)


has : List Selector -> Single msg -> Expectation
has =
    Test.Html.Query.has
