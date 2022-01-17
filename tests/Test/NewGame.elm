module Test.NewGame exposing (..)

import Element
import Fuzz
import NewGame
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
                NewGame.view
                    { seed = Random.initialSeed 0
                    , civilizationNameSingular = ""
                    , civilizationNamePlural = ""
                    , hasUniquePluralName = True
                    , civilizationNamePossessive = ""
                    , hasUniquePossessiveName = True
                    , homePlanetName = ""
                    }
                    |> .body
                    |> Query.fromElement
                    |> Query.has
                        [ Html.text "Space Sim!"
                        , Html.text "Start Game"
                        ]
        , Test.fuzz Fuzz.string "test singular name" <|
            \singularName ->
                ProgramTest.createDocument
                    { init = NewGame.init
                    , update = NewGame.update
                    , view = View.viewToTestDocument (Element.layout []) NewGame.view
                    }
                    |> ProgramTest.start (Random.initialSeed 0)
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "singular-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.fillIn "" "Civilization Name Singular:" singularName
                    |> ProgramTest.expectView
                        (Query.find [ Html.id "singular-name-example" ]
                            >> Query.has
                                [ Html.text singularName ]
                        )
        , Test.fuzz2 Fuzz.string Fuzz.string "test plural name" <|
            \singularName pluralName ->
                ProgramTest.createDocument
                    { init = NewGame.init
                    , update = NewGame.update
                    , view = View.viewToTestDocument (Element.layout []) NewGame.view
                    }
                    |> ProgramTest.start (Random.initialSeed 0)
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.fillIn "" "Civilization Name Singular:" singularName
                    |> ProgramTest.fillIn "" "Civilization Name Plural:" pluralName
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text pluralName ]
                        )
                    |> ProgramTest.clickButton ("Use '" ++ singularName ++ "' as the plural name")
                    |> ProgramTest.expectView
                        (Query.find [ Html.id "plural-name-example" ]
                            >> Query.has
                                [ Html.text singularName ]
                        )
        , Test.fuzz2 Fuzz.string Fuzz.string "test possessive name" <|
            \singularName possessiveName ->
                ProgramTest.createDocument
                    { init = NewGame.init
                    , update = NewGame.update
                    , view = View.viewToTestDocument (Element.layout []) NewGame.view
                    }
                    |> ProgramTest.start (Random.initialSeed 0)
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "possessive-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.fillIn "" "Civilization Name Singular:" singularName
                    |> ProgramTest.fillIn "" "Civilization Name Possessive:" possessiveName
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "possessive-name-example" ]
                            >> Query.has
                                [ Html.text possessiveName ]
                        )
                    |> ProgramTest.clickButton ("Use '" ++ singularName ++ "' as the possessive name")
                    |> ProgramTest.expectView
                        (Query.find [ Html.id "possessive-name-example" ]
                            >> Query.has
                                [ Html.text singularName ]
                        )
        , Test.fuzz Fuzz.string "test home planet name" <|
            \planetName ->
                ProgramTest.createDocument
                    { init = NewGame.init
                    , update = NewGame.update
                    , view = View.viewToTestDocument (Element.layout []) NewGame.view
                    }
                    |> ProgramTest.start (Random.initialSeed 0)
                    |> ProgramTest.ensureView
                        (Query.find [ Html.id "home-planet-name-example" ]
                            >> Query.has
                                [ Html.text "" ]
                        )
                    |> ProgramTest.fillIn "" "Home Planet Name:" planetName
                    |> ProgramTest.expectView
                        (Query.find [ Html.id "home-planet-name-example" ]
                            >> Query.has
                                [ Html.text planetName ]
                        )
        ]
