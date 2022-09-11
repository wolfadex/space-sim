module Game.Components exposing
    ( CelestialBodyForm(..)
    , CivilizationFocus(..)
    , Happiness
    , LightYear
    , Log
    , Mortality
    , Orbit
    , PlayingMsg(..)
    , Reproduction
    , SpaceFocus(..)
    , TickRate(..)
    , ViewStyle(..)
    , Visible(..)
    , Water
    , World
    , childrenSpec
    , civilizationDensitySpec
    , civilizationHappinessSpec
    , civilizationMortalityRateSpec
    , civilizationPopulationSpec
    , civilizationReproductionRateSpec
    , emptyWorld
    , namedSpec
    , orbitSpec
    , parentSpec
    , planetSizeSpec
    , planetTypeSpec
    , positionSpec
    , waterSpec
    )

import Browser.Dom exposing (Viewport)
import Data.Civilization exposing (CivilizationName)
import Data.Knowledge exposing (Knowledge, KnowledgeTree)
import Data.StarDate exposing (StarDate)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Length exposing (Meters)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Rate exposing (Rate)
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared exposing (PlayType(..), SharedMsg)
import Task.Parallel
import Temperature exposing (Temperature)


type alias World =
    { spaceFocus : SpaceFocus
    , civilizationFocus : CivilizationFocus
    , tickRate : TickRate
    , viewStyle : ViewStyle
    , elapsedTime : Float
    , remainingTimeForSystemUpdate : Float
    , galaxyViewSize : { width : Float, height : Float }
    , zoom : Float
    , viewRotation : Float
    , settingsVisible : Visible
    , playType : PlayType

    ---- ECS stuff
    , ecsInternals : Logic.Entity.Extra.Internals

    -- CIV
    , civilizationPopulations : Logic.Component.Set (Dict EntityID Population)
    , civilizationReproductionRates : Logic.Component.Set (Rate Reproduction)
    , civilizationMortalityRates : Logic.Component.Set (Rate Mortality)
    , civilizationDensity : Logic.Component.Set Float
    , civilizationHappiness : Logic.Component.Set (Dict EntityID (Percent Happiness))
    , civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge)
    , civilizationStyle : Logic.Component.Set Data.Civilization.Characteristics
    , named : Logic.Component.Set CivilizationName

    -- Other
    , planetTypes : Logic.Component.Set CelestialBodyForm
    , starTemperature : Logic.Component.Set Temperature
    , orbits : Logic.Component.Set Orbit
    , waterContent : Logic.Component.Set (Percent Water)
    , planetSize : Logic.Component.Set Float
    , parents : Logic.Component.Set EntityID
    , children : Logic.Component.Set (Set EntityID)
    , galaxyPositions : Logic.Component.Set (Point3d Meters LightYear)

    ---- Book keeping
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , playerCiv : Maybe EntityID
    , civilizations : Set EntityID
    , availableCivilizationNames : List CivilizationName
    , starDate : StarDate
    , eventLog : List Log
    , knowledgeTree : KnowledgeTree
    , buildingKnowledgeState : Task.Parallel.ListState PlayingMsg (List ( Knowledge, List (AnySet String Knowledge) ))
    , buildingKnowledge : Maybe ( Int, Int )
    }


emptyWorld : World
emptyWorld =
    { spaceFocus = FGalaxy
    , civilizationFocus = FAll
    , tickRate = Normal
    , viewStyle = ThreeD
    , elapsedTime = 982374
    , remainingTimeForSystemUpdate = 0
    , galaxyViewSize = { width = 800, height = 600 }
    , zoom = 0
    , viewRotation = 0
    , settingsVisible = Hidden
    , playType = Observation

    --
    , ecsInternals = Logic.Entity.Extra.initInternals
    , named = Logic.Component.empty
    , civilizationReproductionRates = Logic.Component.empty
    , civilizationMortalityRates = Logic.Component.empty
    , civilizationDensity = Logic.Component.empty
    , planetTypes = Logic.Component.empty
    , starTemperature = Logic.Component.empty
    , parents = Logic.Component.empty
    , children = Logic.Component.empty
    , orbits = Logic.Component.empty
    , waterContent = Logic.Component.empty
    , planetSize = Logic.Component.empty
    , civilizationPopulations = Logic.Component.empty
    , civilizationHappiness = Logic.Component.empty
    , civilizationKnowledge = Logic.Component.empty
    , civilizationStyle = Logic.Component.empty
    , galaxyPositions = Logic.Component.empty

    --
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , playerCiv = Nothing
    , civilizations = Set.empty
    , availableCivilizationNames = Data.Civilization.allNames
    , starDate = Data.StarDate.init
    , eventLog = []
    , knowledgeTree = Data.Knowledge.baseKnowledgeTree
    , buildingKnowledgeState =
        Tuple.first
            (Task.Parallel.attemptList
                { onUpdates = BuildingKnowledge
                , onSuccess = KnowledgeBuilt
                , onFailure = \_ -> KnowledgeBuildFailure
                , tasks = []
                }
            )
    , buildingKnowledge = Nothing
    }


type PlayingMsg
    = DeleteGalaxy
    | SetSpaceFocus SpaceFocus
    | SetCivilizationFocus CivilizationFocus
    | Tick Float
    | SetTickRate TickRate
    | GotViewStyle ViewStyle
    | WindowResized
    | GotGalaxyViewport (Result Browser.Dom.Error Viewport)
    | GotZoom Value
    | GotZoomChange Float
    | GotRotationChange Float
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg
    | BuildingKnowledge (Task.Parallel.ListMsg (List ( Knowledge, List (AnySet String Knowledge) )))
    | KnowledgeBuilt (List (List ( Knowledge, List (AnySet String Knowledge) )))
    | KnowledgeBuildFailure


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


type Reproduction
    = Reproduction Never


civilizationMortalityRateSpec : Spec (Rate Mortality) { world | civilizationMortalityRates : Logic.Component.Set (Rate Mortality) }
civilizationMortalityRateSpec =
    Logic.Component.Spec .civilizationMortalityRates (\comps world -> { world | civilizationMortalityRates = comps })


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


civilizationDensitySpec : Spec Float { world | civilizationDensity : Logic.Component.Set Float }
civilizationDensitySpec =
    Logic.Component.Spec .civilizationDensity (\comps world -> { world | civilizationDensity = comps })


positionSpec : Spec (Point3d Meters coordinates) { world | galaxyPositions : Logic.Component.Set (Point3d Meters coordinates) }
positionSpec =
    Logic.Component.Spec .galaxyPositions (\comps world -> { world | galaxyPositions = comps })


type LightYear
    = LightYear Never
