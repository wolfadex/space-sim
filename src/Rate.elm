module Rate exposing
    ( Rate
    , fromFloat
    , random
    , toFloat
    )

import Quantity exposing (Quantity(..))
import Random exposing (Generator)


type alias Rate a =
    Quantity Float a


fromFloat : Float -> Rate a
fromFloat =
    Quantity


toFloat : Rate a -> Float
toFloat (Quantity a) =
    a


random : Float -> Float -> Generator (Rate a)
random min max =
    Random.map fromFloat
        (Random.float min max)
