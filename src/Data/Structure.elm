module Data.Structure exposing
    ( Structure
    , Type(..)
    , civilizationStructuresSpec
    , random
    , toString
    )

import Data.StarDate exposing (StarDate)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Random exposing (Generator)


civilizationStructuresSpec : Spec Structure { world | civilizationStructures : Logic.Component.Set Structure }
civilizationStructuresSpec =
    Logic.Component.Spec .civilizationStructures (\comps world -> { world | civilizationStructures = comps })


type alias Structure =
    { creators : EntityID
    , creationDate : StarDate
    , type_ : Type
    , planet : EntityID
    }


type Type
    = Monument
    | City
    | Excavation -- canal, built island, strip minning, etc
    | TimeCapsule


random : Generator Type
random =
    Random.uniform Monument
        [ City
        , Excavation
        , TimeCapsule
        ]


toString : Structure -> String
toString structure =
    case structure.type_ of
        Monument ->
            "Monument"

        City ->
            "City"

        Excavation ->
            "Excavation"

        TimeCapsule ->
            "TimeCapsule"
