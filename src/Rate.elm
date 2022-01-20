module Rate exposing
    ( Rate
    , fromRate
    , random
    , toRate
    )

import Quantity exposing (Quantity(..))
import Random exposing (Generator)


type alias Rate a =
    Quantity Float a


toRate : Float -> Rate a
toRate =
    Quantity


fromRate : Rate a -> Float
fromRate (Quantity a) =
    a


random : Float -> Float -> Generator (Rate a)
random min max =
    Random.map toRate
        (Random.float min max)
