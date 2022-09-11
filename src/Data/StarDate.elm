module Data.StarDate exposing
    ( StarDate
    , distance
    , increment
    , init
    , toString
    )

import Numeral


{-| Roughly 10 Earth years
-}
type StarDate
    = StarDate Int


init : StarDate
init =
    StarDate 0


increment : StarDate -> StarDate
increment (StarDate d) =
    StarDate (d + 1)


toString : StarDate -> String
toString (StarDate d) =
    "Star Date " ++ Numeral.format "0,0.0" (toFloat d / 10)


distance : StarDate -> StarDate -> Int
distance (StarDate left) (StarDate right) =
    abs (left - right)
