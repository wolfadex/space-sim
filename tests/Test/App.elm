module Test.App exposing (..)

import App exposing (Model(..))
import Element
import ProgramTest
import Random
import Test exposing (Test)
import Test.Element.Query as Query
import Test.Html.Selector as Html
import View


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
        , Test.test "test updating names" <|
            \() ->
                ProgramTest.createDocument
                    { init = App.init
                    , update = App.update
                    , view = View.viewToTestDocument (Element.layout []) App.view
                    }
                    |> ProgramTest.start { seed0 = 0 }
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "singular-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.fillIn "singular-name" "" "Carl"
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "singular-name-example" ]
                            >> Query.has
                                [ Html.text "Carl" ]
                        )
                    |> ProgramTest.fillIn "plural-name" "" "Karls"
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text "Karls" ]
                        )
                    |> ProgramTest.clickButton "Use 'Carl' as the plural name"
                    |> ProgramTest.expectView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text "Carl" ]
                        )
        ]
