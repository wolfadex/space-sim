module Population
    ( Population
    , PopulationInTrillions
    , billion
    , inBillions
    , inMillions
    , inTrillions
    , million
    , millions
    , trillion
    ) where

import Units.Quantity (Quantity(..))
import qualified Units.Quantity as Quantity


type Population = Quantity PopulationInTrillions


data PopulationInTrillions = PopulationInTrillions
    deriving (Show)


millions :: Float -> Population
millions population =
    Quantity (population / 1000000)


inMillions :: Population -> Float
inMillions (Quantity population) =
    population * 1000000


inBillions :: Population -> Float
inBillions (Quantity population) =
    population * 1000


inTrillions :: Population -> Float
inTrillions (Quantity population) =
    population


million :: Population
million =
    Quantity 0.000001


billion :: Population
billion =
    Quantity 0.001


trillion :: Population
trillion =
    Quantity 1.0
