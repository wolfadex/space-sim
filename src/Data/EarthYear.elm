module Data.EarthYear exposing
    ( EarthYear
    , EarthYears
    , distance
    , earthYears
    , formatAsStarDate
    , inEarthYears
    , starDates
    )

{-| A "Star Date" is eastimated at 10 Earth years
-}

import Numeral
import Quantity exposing (Quantity(..))


type alias EarthYear =
    Quantity Float EarthYears


type EarthYears
    = EarthYears Never


earthYears : Float -> EarthYear
earthYears =
    Quantity


inEarthYears : EarthYear -> Float
inEarthYears (Quantity ey) =
    ey


starDates : Float -> EarthYear
starDates i =
    earthYears (i * 10)


formatAsStarDate : EarthYear -> String
formatAsStarDate (Quantity ey) =
    "Star Date " ++ Numeral.format "0,0.0" (ey / 100)


distance : EarthYear -> EarthYear -> Float
distance (Quantity left) (Quantity right) =
    abs (left - right)
