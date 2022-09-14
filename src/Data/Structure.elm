module Data.Structure exposing
    ( Structure
    , Type(..)
    , civilizationStructuresSpec
    , random
    , toString
    )

import Data.EarthYear exposing (EarthYear)
import Data.Name exposing (Name)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Random exposing (Generator)


civilizationStructuresSpec : Spec Structure { world | civilizationStructures : Logic.Component.Set Structure }
civilizationStructuresSpec =
    Logic.Component.Spec .civilizationStructures (\comps world -> { world | civilizationStructures = comps })


type alias Structure =
    { creators : EntityID
    , creationDate : EarthYear
    , type_ : Type
    , planet : EntityID
    , name : String
    }


type Type
    = Monument
    | City
    | Excavation -- canal, built island, strip minning, etc
    | TimeCapsule


random : Name -> Generator ( Type, String )
random name =
    Random.andThen
        (\( type_, nameGen ) ->
            Random.map (Tuple.pair type_)
                nameGen
        )
        (Random.uniform ( Monument, Random.constant ("Tower of " ++ Data.Name.toString name) )
            [ ( City, Random.constant ("City of " ++ Data.Name.toString name) )
            , ( Excavation
              , Random.uniform (Data.Name.toString name ++ " Canal")
                    [ Data.Name.toString name ++ " Island"
                    , "Plateau of " ++ Data.Name.toString name
                    ]
              )
            , ( TimeCapsule, Random.constant ("A time capsule belonging to " ++ Data.Name.toString name) )
            ]
        )


toString : Structure -> String
toString structure =
    structure.name
