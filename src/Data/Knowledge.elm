module Data.Knowledge exposing
    ( Knowledge(..)
    , comparableConfig
    , doesntKnow
    , knows
    )

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


knows : AnySet String Knowledge -> Knowledge -> Bool
knows civKnowledge k =
    Set.Any.member comparableConfig k civKnowledge


doesntKnow : AnySet String Knowledge -> Knowledge -> Bool
doesntKnow civKnowledge k =
    not (Set.Any.member comparableConfig k civKnowledge)


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

                _ ->
                    BasicAgriculture
    }
