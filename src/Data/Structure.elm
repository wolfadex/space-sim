module Data.Structure exposing
    ( Structure
    , Type(..)
    , civilizationStructuresSpec
    , random
    , toString
    )

import Data.Name exposing (PersonName)
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
    , name : String
    }


type Type
    = Monument
    | City
    | Excavation -- canal, built island, strip minning, etc
    | TimeCapsule


random : PersonName -> Generator ( Type, String )
random name =
    Random.andThen
        (\( type_, nameGen ) ->
            Random.map (Tuple.pair type_)
                nameGen
        )
        (Random.uniform ( Monument, Random.constant ("Tower of " ++ name) )
            [ ( City, Random.constant ("City of " ++ name) )
            , ( Excavation
              , Random.uniform (name ++ " Canal")
                    [ name ++ " Island"
                    , "Plateau of " ++ name
                    ]
              )
            , ( TimeCapsule, Random.constant ("A time capsule belonging to " ++ name) )
            ]
        )


toString : Structure -> String
toString structure =
    structure.name
