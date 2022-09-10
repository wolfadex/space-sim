module Population exposing
    ( Population
    , PopulationInTrillions
    , billion
    , billions
    , difference
    , inBillions
    , inMillions
    , inTrillions
    , million
    , millions
    , multiplyBy
    , plus
    , trillion
    , trillions
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


billions : Float -> Population
billions population =
    Quantity (population / 1000)


inBillions : Population -> Float
inBillions (Quantity population) =
    population * 1000


trillions : Float -> Population
trillions population =
    Quantity population


inTrillions : Population -> Float
inTrillions (Quantity population) =
    population


million : Population
million =
    Quantity 0.000001


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


difference : Population -> Population -> Population
difference =
    Quantity.difference
