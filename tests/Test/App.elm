module Test.App exposing (..)

import App exposing (Focus(..))
import Random
import Test exposing (Test)
import Test.Element.Query as Query
import Test.Html.Selector as Html


suite : Test
suite =
    Test.describe "Renders the game"
        [ Test.test "renders and runs the app view" <|
            \_ ->
                App.view
                    { seed = Random.initialSeed 0
                    , galaxy = App.exmptyGalaxy
                    , focus = FGalaxy
                    }
                    |> .body
                    |> Query.fromElement
                    |> Query.has [ Html.text "Generate", Html.text "Delete" ]
        ]
