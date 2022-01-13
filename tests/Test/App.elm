module Test.App exposing (..)

import App exposing (Model(..))
import Random
import Test exposing (Test)
import Test.Element.Query as Query
import Test.Html.Selector as Html
import Testing


suite : Test
suite =
    Test.describe "Renders the game"
        [ Test.test "renders and runs the new game view" <|
            \() ->
                App.view
                    (NewGame
                        { seed = Random.initialSeed 0
                        , civilizationNameSingular = ""
                        , civilizationNamePlural = ""
                        , hasUniquePluralName = True
                        }
                    )
                    |> .body
                    |> Query.fromElement
                    |> Query.has
                        [ Html.text "Space Sim!"
                        , Html.text "Start Game"
                        ]
        , Test.test "shows the correct plural name" <|
            \() ->
                App.view
                    (NewGame
                        { seed = Random.initialSeed 0
                        , civilizationNameSingular = "Carl"
                        , civilizationNamePlural = "Karls"
                        , hasUniquePluralName = True
                        }
                    )
                    |> .body
                    |> Query.fromElement
                    |> Query.find [ Html.attribute (Testing.testHtmlAttribute "plural-name") ]
                    |> Query.has [ Html.text "Karls" ]
        , Test.test "shows the correct singular name" <|
            \() ->
                App.view
                    (NewGame
                        { seed = Random.initialSeed 0
                        , civilizationNameSingular = "Carl"
                        , civilizationNamePlural = "Karls"
                        , hasUniquePluralName = True
                        }
                    )
                    |> .body
                    |> Query.fromElement
                    |> Query.find [ Html.attribute (Testing.testHtmlAttribute "singular-name") ]
                    |> Query.has
                        [ Html.text "Carl" ]
        ]
