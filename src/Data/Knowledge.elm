module Data.Knowledge exposing
    ( Knowledge(..)
    , canBeLearned
    , comparableConfig
    , knows
    )

import Dict.Any exposing (AnyDict)
import Logic.Entity exposing (EntityID)
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
      -- Things in the universe
    | KnowsOf EntityID


knows : AnySet String Knowledge -> Knowledge -> Bool
knows civKnowledge k =
    Set.Any.member comparableConfig k civKnowledge


comparableConfig : { toComparable : Knowledge -> String, fromComparable : String -> Knowledge }
comparableConfig =
    { toComparable =
        \knowledge ->
            case knowledge of
                LandTravel ->
                    "LandTravel"

                WaterSurfaceTravel ->
                    "WaterSurfaceTravel"

                UnderwaterTravel ->
                    "UnderwaterTravel"

                Flight ->
                    "Flight"

                PlanetarySpaceTravel ->
                    "PlanetarySpaceTravel"

                InterplanetarySpaceTravel ->
                    "InterplanetarySpaceTravel"

                FTLSpaceTravel ->
                    "FTLSpaceTravel"

                BasicAgriculture ->
                    "BasicAgriculture"

                BasicMetalWorking ->
                    "BasicMetalWorking"

                Optics ->
                    "Optics"

                KnowsOf id ->
                    "KnowsOf__" ++ String.fromInt id
    , fromComparable =
        \str ->
            case str of
                "LandTravel" ->
                    LandTravel

                "WaterSurfaceTravel" ->
                    WaterSurfaceTravel

                "UnderwaterTravel" ->
                    UnderwaterTravel

                "Flight" ->
                    Flight

                "PlanetarySpaceTravel" ->
                    PlanetarySpaceTravel

                "InterplanetarySpaceTravel" ->
                    InterplanetarySpaceTravel

                "FTLSpaceTravel" ->
                    FTLSpaceTravel

                "BasicAgriculture" ->
                    BasicAgriculture

                "BasicMetalWorking" ->
                    BasicMetalWorking

                "Optics" ->
                    Optics

                other ->
                    if String.startsWith "KnowsOf__" other then
                        case String.toInt (String.dropLeft 9 other) of
                            Just id ->
                                KnowsOf id

                            Nothing ->
                                BasicAgriculture

                    else
                        BasicAgriculture
    }


{-| Given a set of known things, returns a set of things that can be learned
-}
canBeLearned : AnySet String Knowledge -> AnySet String Knowledge
canBeLearned known =
    Dict.Any.filter comparableConfig
        (\_ requirements ->
            List.isEmpty requirements
                || List.any
                    (\setOfRequirements ->
                        Set.Any.isEmpty (Set.Any.diff setOfRequirements known)
                    )
                    requirements
        )
        requiredBeforeLearning
        |> Dict.Any.keys comparableConfig
        |> Set.Any.fromList comparableConfig


{-| Returns what the required `Knoweldge` is to learn a new thing. An empty `List` means that no prior `Knowledge` is required. If more than 1 set of `Knowledge` is returns, then any of those sets can be met.
-}
requiredBeforeLearning : AnyDict String Knowledge (List (AnySet String Knowledge))
requiredBeforeLearning =
    Dict.Any.fromList comparableConfig
        [ ( LandTravel, [] )
        , ( WaterSurfaceTravel, [] )
        , ( UnderwaterTravel, [] )
        , ( BasicAgriculture, [] )
        , ( BasicMetalWorking, [] )
        , ( Optics, [ Set.Any.singleton comparableConfig BasicMetalWorking ] )
        , ( Flight
          , [ Set.Any.singleton comparableConfig LandTravel
            , Set.Any.singleton comparableConfig WaterSurfaceTravel
            ]
          )
        , ( PlanetarySpaceTravel, [ Set.Any.singleton comparableConfig Flight ] )
        , ( KnowsOf -1, [ Set.Any.singleton comparableConfig Optics ] )
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



-- LandTravel
-- | WaterSurfaceTravel
-- | UnderwaterTravel
-- | Flight
-- | PlanetarySpaceTravel
-- | InterplanetarySpaceTravel
-- | FTLSpaceTravel
--     -- Basics of civilization
-- | BasicAgriculture
-- | BasicMetalWorking
-- | Optics
--     -- Things in the universe
-- | KnowsOf EntityID
