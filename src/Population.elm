module Population exposing
    ( Population
    , PopulationInTrillions
    , billion
    , inBillions
    , inMillions
    , inTrillions
    , millions
    , multiplyBy
    , plus
    , trillion
    )

import Quantity exposing (Quantity(..))


type alias Population =
    Quantity Float PopulationInTrillions


type PopulationInTrillions
    = PopulationInTrillions Never


millions : Float -> Population
millions population =
    Quantity (population / 1000000)


inMillions : Population -> Float
inMillions (Quantity population) =
    population * 1000000


inBillions : Population -> Float
inBillions (Quantity population) =
    population * 1000


inTrillions : Population -> Float
inTrillions (Quantity population) =
    population


billion : Population
billion =
    Quantity 0.001


trillion : Population
trillion =
    Quantity 1.0


multiplyBy : Float -> Population -> Population
multiplyBy =
    Quantity.multiplyBy


plus : Population -> Population -> Population
plus =
    Quantity.plus
