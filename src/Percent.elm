module Percent exposing
    ( Percent
    , fromFloat
    , oneHundred
    , random
    , toFloat
    , zero
    )

import Quantity exposing (Quantity(..))
import Random exposing (Generator)


type alias Percent a =
    Quantity Float a


{-| 1.0 is equivalent to 100%
-}
fromFloat : Float -> Percent a
fromFloat p =
    Quantity (pMin (pMax p))


toFloat : Percent a -> Float
toFloat (Quantity a) =
    a


random : Float -> Float -> Generator (Percent a)
random min max =
    Random.map fromFloat
        (Random.float (pMin (pMax min)) (pMin (pMax max)))


zero : Percent a
zero =
    Quantity 0.0


oneHundred : Percent a
oneHundred =
    Quantity 1.0


pMin : Float -> Float
pMin p =
    max 0 p


pMax : Float -> Float
pMax p =
    min 1 p
