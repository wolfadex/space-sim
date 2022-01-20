module Percent exposing
    ( Percent
    , fromFloat
    , random
    , toFloat
    , zero
    )

import Quantity exposing (Quantity(..))
import Random exposing (Generator)


type alias Percent a =
    Quantity Float a


{-| 100.0 is equivalent to 100%
-}
fromFloat : Float -> Percent a
fromFloat =
    Quantity


toFloat : Percent a -> Float
toFloat (Quantity a) =
    a


random : Float -> Float -> Generator (Percent a)
random min max =
    Random.map fromFloat
        (Random.float min max)


zero : Percent a
zero =
    Quantity 0.0
