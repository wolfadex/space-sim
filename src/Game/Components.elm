module Game.Components exposing
    ( CelestialBodyForm(..)
    , CivilizationReproductionRate
    , Name
    , Orbit
    , ScaledNumber(..)
    , StarSize(..)
    , Water
    , celestialBodySpec
    , childrenSpec
    , civilizationReproductionRateSpec
    , civilizationSizeSpec
    , namedSpec
    , orbitSpec
    , parentSpec
    , planetSizeSpec
    , scaledMultiply
    , starFormSpec
    , waterSpec
    )

import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Set exposing (Set)


civilizationReproductionRateSpec : Spec CivilizationReproductionRate { world | civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate }
civilizationReproductionRateSpec =
    Logic.Component.Spec .civilizationReproductionRates (\comps world -> { world | civilizationReproductionRates = comps })


type alias CivilizationReproductionRate =
    Float


civilizationSizeSpec : Spec ScaledNumber { world | civilizationSizes : Logic.Component.Set ScaledNumber }
civilizationSizeSpec =
    Logic.Component.Spec .civilizationSizes (\comps world -> { world | civilizationSizes = comps })


type ScaledNumber
    = Millions Float
    | Billions Float


scaledMultiply : Float -> ScaledNumber -> ScaledNumber
scaledMultiply n scaled =
    case scaled of
        Millions f ->
            let
                product : Float
                product =
                    n * f
            in
            if String.length (String.fromInt (floor product)) > 10 then
                Billions (product / 1000)

            else
                Millions product

        Billions f ->
            let
                product : Float
                product =
                    n * f
            in
            if String.length (String.fromInt (floor product)) < 2 then
                Millions (product * 1000)

            else
                Billions product


namedSpec : Spec Name { world | named : Logic.Component.Set Name }
namedSpec =
    Logic.Component.Spec .named (\comps world -> { world | named = comps })


type alias Name =
    { singular : String
    , plural : Maybe String
    }


celestialBodySpec : Spec CelestialBodyForm { world | celestialBodyForms : Logic.Component.Set CelestialBodyForm }
celestialBodySpec =
    Logic.Component.Spec .celestialBodyForms (\comps world -> { world | celestialBodyForms = comps })


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
