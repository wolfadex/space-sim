module Game.Components exposing
    ( CelestialBodyForm(..)
    , CivilizationReproductionRate
    , Knowledge(..)
    , LightYear
    , Orbit
    , StarSize(..)
    , Water
    , childrenSpec
    , civilizationHappinessSpec
    , civilizationPopulationSpec
    , civilizationReproductionRateSpec
    , knowledgeComparableConfig
    , knowledgeSpec
    , namedSpec
    , orbitSpec
    , parentSpec
    , planetSizeSpec
    , planetTypeSpec
    , positionSpec
    , starFormSpec
    , waterSpec
    )

import Data.Names exposing (CivilizationName)
import Dict exposing (Dict)
import Length exposing (Meters)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Point3d exposing (Point3d)
import ScaledNumber exposing (ScaledNumber)
import Set exposing (Set)
import Set.Any exposing (AnySet)


civilizationReproductionRateSpec : Spec CivilizationReproductionRate { world | civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate }
civilizationReproductionRateSpec =
    Logic.Component.Spec .civilizationReproductionRates (\comps world -> { world | civilizationReproductionRates = comps })


type alias CivilizationReproductionRate =
    Float


namedSpec : Spec CivilizationName { world | named : Logic.Component.Set CivilizationName }
namedSpec =
    Logic.Component.Spec .named (\comps world -> { world | named = comps })


planetTypeSpec : Spec CelestialBodyForm { world | planetTypes : Logic.Component.Set CelestialBodyForm }
planetTypeSpec =
    Logic.Component.Spec .planetTypes (\comps world -> { world | planetTypes = comps })


type CelestialBodyForm
    = Rocky
    | Gas


orbitSpec : Spec Orbit { world | orbits : Logic.Component.Set Orbit }
orbitSpec =
    Logic.Component.Spec .orbits (\comps world -> { world | orbits = comps })


type alias Orbit =
    Int


starFormSpec : Spec StarSize { world | starForms : Logic.Component.Set StarSize }
starFormSpec =
    Logic.Component.Spec .starForms (\comps world -> { world | starForms = comps })


type StarSize
    = Yellow
    | RedGiant
    | BlueGiant
    | WhiteDwarf
    | BlackDwarf


parentSpec : Spec EntityID { world | parents : Logic.Component.Set EntityID }
parentSpec =
    Logic.Component.Spec .parents (\comps world -> { world | parents = comps })


childrenSpec : Spec (Set EntityID) { world | children : Logic.Component.Set (Set EntityID) }
childrenSpec =
    Logic.Component.Spec .children (\comps world -> { world | children = comps })


waterSpec : Spec Water { world | waterContent : Logic.Component.Set Water }
waterSpec =
    Logic.Component.Spec .waterContent (\comps world -> { world | waterContent = comps })


planetSizeSpec : Spec Float { world | planetSize : Logic.Component.Set Float }
planetSizeSpec =
    Logic.Component.Spec .planetSize (\comps world -> { world | planetSize = comps })


type alias Water =
    Float


civilizationPopulationSpec : Spec (Dict EntityID ScaledNumber) { world | civilizationPopulations : Logic.Component.Set (Dict EntityID ScaledNumber) }
civilizationPopulationSpec =
    Logic.Component.Spec .civilizationPopulations (\comps world -> { world | civilizationPopulations = comps })


civilizationHappinessSpec : Spec Float { world | civilizationHappiness : Logic.Component.Set Float }
civilizationHappinessSpec =
    Logic.Component.Spec .civilizationHappiness (\comps world -> { world | civilizationHappiness = comps })


knowledgeSpec : Spec (AnySet String Knowledge) { world | civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge) }
knowledgeSpec =
    Logic.Component.Spec .civilizationKnowledge (\comps world -> { world | civilizationKnowledge = comps })


type Knowledge
    = LandTravel
    | WaterSurfaceTravel
    | UnderwaterTravel
    | Flight
    | PlanetarySpaceTravel
    | InterplanetarySpaceTravel
    | FTLSpaceTravel


knowledgeComparableConfig : { toComparable : Knowledge -> String }
knowledgeComparableConfig =
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
    }


positionSpec : Spec (Point3d Meters coordinates) { world | galaxyPositions : Logic.Component.Set (Point3d Meters coordinates) }
positionSpec =
    Logic.Component.Spec .galaxyPositions (\comps world -> { world | galaxyPositions = comps })


type LightYear
    = LightYear Never
