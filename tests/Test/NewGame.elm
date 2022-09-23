module Test.NewGame exposing (..)

import Element
import Fuzz
import NewGame
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
        [ Test.test "renders the title and start button"
            (\() ->
                Query.has
                    [ Html.text "Space Sim!"
                    , Html.text "Participate"
                    , Html.text "Observe"
                    ]
                    (Query.fromElement (.body (NewGame.view testSharedModel NewGame.baseModel)))
            )
        , Test.describe "Participation view"
            [ Test.fuzz Fuzz.string
                "test singular name"
                (\singularName ->
                    ProgramTest.expectView
                        (Query.find [ Html.id "singular-name-example" ]
                            >> Query.has
                                [ Html.text singularName ]
                        )
                        (ProgramTest.fillIn ""
                            "Name Singular:"
                            singularName
                            (ProgramTest.ensureView
                                (Query.find [ Html.id "singular-name-example" ]
                                    >> Query.has
                                        [ Html.text "" ]
                                )
                                (ProgramTest.clickButton "Participate"
                                    (ProgramTest.start (Random.initialSeed 0)
                                        (ProgramTest.createDocument
                                            { init = \_ -> NewGame.init
                                            , update = NewGame.update testSharedModel
                                            , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                                            }
                                        )
                                    )
                                )
                            )
                        )
                )
            , Test.fuzz Fuzz.string
                "test home planet name"
                (\planetName ->
                    ProgramTest.expectView
                        (Query.find [ Html.id "home-planet-name-example" ]
                            >> Query.has
                                [ Html.text planetName ]
                        )
                        (ProgramTest.fillIn ""
                            "Home Planet Name:"
                            planetName
                            (ProgramTest.ensureView
                                (Query.find [ Html.id "home-planet-name-example" ]
                                    >> Query.has
                                        [ Html.text "" ]
                                )
                                (ProgramTest.clickButton "Participate"
                                    (ProgramTest.start (Random.initialSeed 0)
                                        (ProgramTest.createDocument
                                            { init = \_ -> NewGame.init
                                            , update = NewGame.update testSharedModel
                                            , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                                            }
                                        )
                                    )
                                )
                            )
                        )
                )
            , Test.fuzz2 Fuzz.int
                Fuzz.int
                "test solar system count"
                (\_ _ ->
                    ProgramTest.expectView
                        (Query.find [ Html.id "max-solar-system-count" ]
                            >> Query.has
                                [ Html.text "80" ]
                        )
                        (ProgramTest.ensureView
                            (Query.find [ Html.id "min-solar-system-count" ]
                                >> Query.has
                                    [ Html.text "40" ]
                            )
                            (ProgramTest.clickButton "Observe"
                                (ProgramTest.start (Random.initialSeed 0)
                                    (ProgramTest.createDocument
                                        { init = \_ -> NewGame.init
                                        , update = NewGame.update testSharedModel
                                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                                        }
                                    )
                                )
                            )
                        )
                )
            ]
        , Test.describe "Observation view"
            [ Test.fuzz2 Fuzz.int
                Fuzz.int
                "test  solar system count"
                (\_ _ ->
                    ProgramTest.expectView
                        (Query.find [ Html.id "max-solar-system-count" ]
                            >> Query.has
                                [ Html.text "80" ]
                        )
                        (ProgramTest.ensureView
                            (Query.find [ Html.id "min-solar-system-count" ]
                                >> Query.has
                                    [ Html.text "40" ]
                            )
                            (ProgramTest.clickButton "Observe"
                                (ProgramTest.start (Random.initialSeed 0)
                                    (ProgramTest.createDocument
                                        { init = \_ -> NewGame.init
                                        , update = NewGame.update testSharedModel
                                        , view = View.viewToTestDocument (Element.layout []) (NewGame.view testSharedModel)
                                        }
                                    )
                                )
                            )
                        )
                )
            ]
        ]
