module Game.Components exposing
    ( AstronomicalUnit
    , CelestialBodyForm(..)
    , CivilizationFocus(..)
    , Enabled(..)
    , Happiness
    , LightYear
    , Log
    , Mortality
    , Orbit
    , Reproduction
    , Settings
    , SpaceFocus(..)
    , StarDate
    , StarSize(..)
    , TickRate(..)
    , ViewStyle(..)
    , Visible(..)
    , Water
    , World
    , childrenSpec
    , civilizationHappinessSpec
    , civilizationMortalityRateSpec
    , civilizationPopulationSpec
    , civilizationReproductionRateSpec
    , emptyWorld
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

import Data.Knowledge exposing (Knowledge)
import Data.Names exposing (CivilizationName)
import Dict exposing (Dict)
import Length exposing (Meters)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Random exposing (Seed)
import Rate exposing (Rate)
import Set exposing (Set)
import Set.Any exposing (AnySet)


type alias World =
    { seed : Seed
    , spaceFocus : SpaceFocus
    , civilizationFocus : CivilizationFocus
    , tickRate : TickRate
    , viewStyle : ViewStyle
    , elapsedTime : Float
    , remainingTimeForSystemUpdate : Float
    , galaxyViewSize : { width : Float, height : Float }
    , zoom : Float
    , viewRotation : Float
    , settingsVisible : Visible
    , settings : Settings

    ---- ECS stuff
    , ecsInternals : Logic.Entity.Extra.Internals

    -- CIV
    , civilizationPopulations : Logic.Component.Set (Dict EntityID Population)
    , civilizationReproductionRates : Logic.Component.Set (Rate Reproduction)
    , civilizationMortalityRates : Logic.Component.Set (Rate Mortality)
    , civilizationHappiness : Logic.Component.Set (Dict EntityID (Percent Happiness))
    , civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge)
    , named : Logic.Component.Set CivilizationName

    -- Other
    , planetTypes : Logic.Component.Set CelestialBodyForm
    , starForms : Logic.Component.Set StarSize
    , orbits : Logic.Component.Set Orbit
    , waterContent : Logic.Component.Set (Percent Water)
    , planetSize : Logic.Component.Set Float
    , parents : Logic.Component.Set EntityID
    , children : Logic.Component.Set (Set EntityID)
    , galaxyPositions : Logic.Component.Set (Point3d Meters LightYear)

    ---- Book keeping entities by ID
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , playerCiv : EntityID
    , civilizations : Set EntityID
    , availableCivilizationNames : List CivilizationName
    , starDate : StarDate
    , eventLog : List Log
    }


type alias Settings =
    { realisticLighting : Enabled }


type Enabled
    = Enabled
    | Disabled


emptyWorld : World
emptyWorld =
    { seed = Random.initialSeed 0
    , spaceFocus = FGalaxy
    , civilizationFocus = FAll
    , tickRate = Normal
    , viewStyle = ThreeD
    , elapsedTime = 0
    , remainingTimeForSystemUpdate = 0
    , galaxyViewSize = { width = 800, height = 600 }
    , zoom = 0
    , viewRotation = 0
    , settingsVisible = Visible
    , settings = { realisticLighting = Enabled }

    --
    , ecsInternals = Logic.Entity.Extra.initInternals
    , named = Logic.Component.empty
    , civilizationReproductionRates = Logic.Component.empty
    , civilizationMortalityRates = Logic.Component.empty
    , planetTypes = Logic.Component.empty
    , starForms = Logic.Component.empty
    , parents = Logic.Component.empty
    , children = Logic.Component.empty
    , orbits = Logic.Component.empty
    , waterContent = Logic.Component.empty
    , planetSize = Logic.Component.empty
    , civilizationPopulations = Logic.Component.empty
    , civilizationHappiness = Logic.Component.empty
    , civilizationKnowledge = Logic.Component.empty
    , galaxyPositions = Logic.Component.empty

    --
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , playerCiv = -1
    , civilizations = Set.empty
    , availableCivilizationNames = Data.Names.allCivilizationNames
    , starDate = 0
    , eventLog = []
    }


type Visible
    = Visible
    | Hidden


type ViewStyle
    = TwoD
    | ThreeD


type alias Log =
    { description : String
    , time : StarDate
    , civilizationId : EntityID
    }


type alias StarDate =
    Int


type SpaceFocus
    = FGalaxy
    | FSolarSystem EntityID
    | FStar EntityID
    | FPlanet EntityID


type CivilizationFocus
    = FAll
    | FOne EntityID


type TickRate
    = Paused
    | Normal
    | Fast
    | ExtraFast
    | HalfSpeed


civilizationReproductionRateSpec : Spec (Rate Reproduction) { world | civilizationReproductionRates : Logic.Component.Set (Rate Reproduction) }
civilizationReproductionRateSpec =
    Logic.Component.Spec .civilizationReproductionRates (\comps world -> { world | civilizationReproductionRates = comps })


civilizationMortalityRateSpec : Spec (Rate Mortality) { world | civilizationMortalityRates : Logic.Component.Set (Rate Mortality) }
civilizationMortalityRateSpec =
    Logic.Component.Spec .civilizationMortalityRates (\comps world -> { world | civilizationMortalityRates = comps })


type Reproduction
    = Reproduction Never


type Mortality
    = Mortality Never


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


waterSpec : Spec (Percent Water) { world | waterContent : Logic.Component.Set (Percent Water) }
waterSpec =
    Logic.Component.Spec .waterContent (\comps world -> { world | waterContent = comps })


planetSizeSpec : Spec Float { world | planetSize : Logic.Component.Set Float }
planetSizeSpec =
    Logic.Component.Spec .planetSize (\comps world -> { world | planetSize = comps })


type Water
    = Water Never


civilizationPopulationSpec : Spec (Dict EntityID Population) { world | civilizationPopulations : Logic.Component.Set (Dict EntityID Population) }
civilizationPopulationSpec =
    Logic.Component.Spec .civilizationPopulations (\comps world -> { world | civilizationPopulations = comps })


civilizationHappinessSpec : Spec (Dict EntityID (Percent Happiness)) { world | civilizationHappiness : Logic.Component.Set (Dict EntityID (Percent Happiness)) }
civilizationHappinessSpec =
    Logic.Component.Spec .civilizationHappiness (\comps world -> { world | civilizationHappiness = comps })


type Happiness
    = Happiness Never


knowledgeSpec : Spec (AnySet String Knowledge) { world | civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge) }
knowledgeSpec =
    Logic.Component.Spec .civilizationKnowledge (\comps world -> { world | civilizationKnowledge = comps })


positionSpec : Spec (Point3d Meters coordinates) { world | galaxyPositions : Logic.Component.Set (Point3d Meters coordinates) }
positionSpec =
    Logic.Component.Spec .galaxyPositions (\comps world -> { world | galaxyPositions = comps })


type LightYear
    = LightYear Never


type AstronomicalUnit
    = AstronomicalUnit Never
