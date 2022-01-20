module Percent exposing
    ( Percent
    , fromPercent
    , random
    , toPercent
    )

import Quantity exposing (Quantity(..))
import Random exposing (Generator)


type alias Percent a =
    Quantity Float a


toPercent : Float -> Percent a
toPercent =
    Quantity


fromPercent : Percent a -> Float
fromPercent (Quantity a) =
    a


random : Float -> Float -> Generator (Percent a)
random min max =
    Random.map toPercent
        (Random.float min max)
