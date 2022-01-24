module Test.NewGame exposing (..)

import Element
import Fuzz
import NewGame exposing (Model(..))
import ProgramTest
import Random
import Shared exposing (SharedModel)
import Test exposing (Test)
import Test.Element.Query as Query
import Test.Html.Selector as Html
import View


testSharedModel : SharedModel
testSharedModel =
    { seed = Random.initialSeed 0
    , settings = Shared.defaultSettings
    }


suite : Test
suite =
    Test.describe "Renders the screen"
        [ Test.test "renders the title and start button" <|
            \() ->
                NewGame.view testSharedModel (MainMenu {})
                    |> .body
                    |> Query.fromElement
                    |> Query.has
                        [ Html.text "Space Sim!"
                        , Html.text "Participate"
                        , Html.text "Observe"
                        ]
        , Test.describe "Participation view"
            [ Test.fuzz Fuzz.string "test singular name" <|
                \singularName ->
                    ProgramTest.createDocument
                        { init = \_ -> NewGame.init
                        , update = NewGame.update testSharedModel
                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                        }
                        |> ProgramTest.start (Random.initialSeed 0)
                        |> ProgramTest.clickButton "Participate"
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
                        { init = \_ -> NewGame.init
                        , update = NewGame.update testSharedModel
                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                        }
                        |> ProgramTest.start (Random.initialSeed 0)
                        |> ProgramTest.clickButton "Participate"
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
                        { init = \_ -> NewGame.init
                        , update = NewGame.update testSharedModel
                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                        }
                        |> ProgramTest.start (Random.initialSeed 0)
                        |> ProgramTest.clickButton "Participate"
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
                        { init = \_ -> NewGame.init
                        , update = NewGame.update testSharedModel
                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                        }
                        |> ProgramTest.start (Random.initialSeed 0)
                        |> ProgramTest.clickButton "Participate"
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
        ]
