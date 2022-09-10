module Data.Knowledge exposing
    ( Knowledge(..)
    , KnowledgeTree
    , addKnowledge
    , baseKnowledgeTree
    , canBeLearned
    , comparableConfig
    , knows
    , spec
    )

import Dict.Any exposing (AnyDict)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Serialize exposing (Codec)
import Set.Any exposing (AnySet)


type
    Knowledge
    -- Modes of travel
    = LandTravel
    | WaterSurfaceTravel
    | UnderwaterTravel
    | Flight
    | PlanetarySpaceTravel
    | InterplanetarySpaceTravel
    | FTLSpaceTravel
      -- Basics of civilization
    | BasicAgriculture
    | BasicMetalWorking
    | Optics
    | Villages
    | Cities
    | MegaCities
      -- Things in the universe
    | KnowsOf EntityID


codec : Codec e Knowledge
codec =
    Serialize.finishCustomType
        (Serialize.variant1 KnowsOf
            Serialize.int
            (Serialize.variant0 MegaCities
                (Serialize.variant0 Cities
                    (Serialize.variant0 Villages
                        (Serialize.variant0 Optics
                            (Serialize.variant0 BasicMetalWorking
                                (Serialize.variant0 BasicAgriculture
                                    (Serialize.variant0 FTLSpaceTravel
                                        (Serialize.variant0 InterplanetarySpaceTravel
                                            (Serialize.variant0 PlanetarySpaceTravel
                                                (Serialize.variant0 Flight
                                                    (Serialize.variant0 UnderwaterTravel
                                                        (Serialize.variant0 WaterSurfaceTravel
                                                            (Serialize.variant0 LandTravel
                                                                (Serialize.customType
                                                                    (\landTravelE waterSurfaceTravelE underwaterTravelE flightE planetarySpaceTravelE interplanetarySpaceTravelE fTLSpaceTravelE basicAgricultureE basicMetalWorkingE opticsE villagesE citiesE megaCitiesE knowsOfE val ->
                                                                        case val of
                                                                            LandTravel ->
                                                                                landTravelE

                                                                            WaterSurfaceTravel ->
                                                                                waterSurfaceTravelE

                                                                            UnderwaterTravel ->
                                                                                underwaterTravelE

                                                                            Flight ->
                                                                                flightE

                                                                            PlanetarySpaceTravel ->
                                                                                planetarySpaceTravelE

                                                                            InterplanetarySpaceTravel ->
                                                                                interplanetarySpaceTravelE

                                                                            FTLSpaceTravel ->
                                                                                fTLSpaceTravelE

                                                                            BasicAgriculture ->
                                                                                basicAgricultureE

                                                                            BasicMetalWorking ->
                                                                                basicMetalWorkingE

                                                                            Optics ->
                                                                                opticsE

                                                                            Villages ->
                                                                                villagesE

                                                                            Cities ->
                                                                                citiesE

                                                                            MegaCities ->
                                                                                megaCitiesE

                                                                            KnowsOf e ->
                                                                                knowsOfE e
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


knows : AnySet String Knowledge -> Knowledge -> Bool
knows civKnowledge k =
    Set.Any.member comparableConfig k civKnowledge


comparableConfig : { toComparable : Knowledge -> String, fromComparable : String -> Knowledge }
comparableConfig =
    { toComparable = Serialize.encodeToString codec
    , fromComparable =
        \str ->
            case Serialize.decodeFromString codec str of
                Ok k ->
                    k

                Err _ ->
                    BasicAgriculture
    }


{-| Given a set of known things, returns a set of things that can be learned
-}
canBeLearned : KnowledgeTree -> AnySet String Knowledge -> AnySet String Knowledge
canBeLearned (KnowledgeTree knowledgeTree) known =
    Set.Any.fromList comparableConfig
        (Dict.Any.keys comparableConfig
            (Dict.Any.filter comparableConfig
                (\_ requirements ->
                    List.isEmpty requirements
                        || List.any
                            (\setOfRequirements ->
                                Set.Any.isEmpty (Set.Any.diff setOfRequirements known)
                            )
                            requirements
                )
                knowledgeTree
            )
        )


type KnowledgeTree
    = KnowledgeTree (AnyDict String Knowledge (List (AnySet String Knowledge)))


{-| Returns what the required `Knoweldge` is to learn a new thing. An empty `List` means that no prior `Knowledge` is required. If more than 1 set of `Knowledge` is returns, then any of those sets can be met.
-}
baseKnowledgeTree : KnowledgeTree
baseKnowledgeTree =
    KnowledgeTree
        (Dict.Any.fromList comparableConfig
            -- Fixed knowledge
            ([ ( LandTravel, [] )
             , ( WaterSurfaceTravel, [] )
             , ( UnderwaterTravel, [] )
             , ( BasicAgriculture, [] )
             , ( BasicMetalWorking, [] )
             , ( Villages, [] )
             , ( Cities, [ Set.Any.singleton comparableConfig Villages ] )
             , ( MegaCities, [ Set.Any.singleton comparableConfig Cities ] )
             , ( Optics, [ Set.Any.singleton comparableConfig BasicMetalWorking ] )
             , ( Flight
               , [ Set.Any.singleton comparableConfig LandTravel
                 , Set.Any.singleton comparableConfig WaterSurfaceTravel
                 ]
               )
             , ( PlanetarySpaceTravel, [ Set.Any.singleton comparableConfig Flight ] )
             , ( InterplanetarySpaceTravel, [ Set.Any.singleton comparableConfig PlanetarySpaceTravel ] )
             , ( InterplanetarySpaceTravel
               , [ Set.Any.fromList comparableConfig
                    [ PlanetarySpaceTravel
                    , BasicAgriculture
                    , Optics
                    ]
                 ]
               )
             , ( FTLSpaceTravel, [ Set.Any.fromList comparableConfig [ InterplanetarySpaceTravel ] ] )
             ]
             -- [( KnowsOf -1, [ Set.Any.singleton comparableConfig Optics ] )]
            )
        )


addKnowledge : List ( Knowledge, List (AnySet String Knowledge) ) -> KnowledgeTree -> KnowledgeTree
addKnowledge newKnowledge (KnowledgeTree existingKnowledge) =
    KnowledgeTree (Dict.Any.union (Dict.Any.fromList comparableConfig newKnowledge) existingKnowledge)


spec : Spec (AnySet String Knowledge) { world | civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge) }
spec =
    Logic.Component.Spec .civilizationKnowledge (\comps world -> { world | civilizationKnowledge = comps })
