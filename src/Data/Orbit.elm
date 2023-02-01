module Data.Orbit exposing
    ( Orbit
    , create
    , distance, period
    )

import Data.EarthYear exposing (EarthYear)
import Length exposing (Length)


type Orbit
    = Orbit Internal


create : { distance : Length, period : EarthYear } -> Orbit
create config =
    Orbit { distance = config.distance, period = config.period }


type alias Internal =
    { distance : Length
    , period : EarthYear
    }


distance : Orbit -> Length
distance (Orbit internal) =
    internal.distance


period : Orbit -> EarthYear
period (Orbit internal) =
    internal.period
