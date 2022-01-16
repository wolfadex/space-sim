module Game.Components exposing
    ( CelestialBodyForm(..)
    , CivilizationReproductionRate
    , Name
    , Orbit
    , StarSize(..)
    , Water
    , childrenSpec
    , civilizationHappinessSpec
    , civilizationPopulationSpec
    , civilizationReproductionRateSpec
    , namedSpec
    , orbitSpec
    , parentSpec
    , planetSizeSpec
    , planetTypeSpec
    , starFormSpec
    , waterSpec
    )

import Dict exposing (Dict)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import ScaledNumber exposing (ScaledNumber)
import Set exposing (Set)


civilizationReproductionRateSpec : Spec CivilizationReproductionRate { world | civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate }
civilizationReproductionRateSpec =
    Logic.Component.Spec .civilizationReproductionRates (\comps world -> { world | civilizationReproductionRates = comps })


type alias CivilizationReproductionRate =
    Float


namedSpec : Spec Name { world | named : Logic.Component.Set Name }
namedSpec =
    Logic.Component.Spec .named (\comps world -> { world | named = comps })


type alias Name =
    { singular : String
    , plural : Maybe String
    }


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
