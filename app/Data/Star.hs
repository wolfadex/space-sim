module Data.Star
    ( generate
    ) where

import qualified Random
import Flow
import System.Random (Random, RandomGen)
import qualified System.Random as Random
import Units.Temperature (Temperature)
import qualified Units.Temperature as Temperature


-- The temperature range is weighted based on abundance, and then a random temp for that abundance's range is generated
generate :: RandomGen g => g -> (Temperature, g)
generate seed =
  (Temperature.kelvins temp, finalSeed)
  where
    (tempGen, nextSeed ) = Random.weighted seed
                            ( 0.00001, Random.randomR (33000, 42000) )
                            [ ( 0.1, Random.randomR (10000, 32999) )
                            , ( 0.7, Random.randomR (7500, 19999) )
                            , ( 2.0, Random.randomR (6000, 7499) )
                            , ( 3.5, Random.randomR (5200, 6999) )
                            , ( 8.0, Random.randomR (3700, 5199) )
                            , ( 80.0, Random.randomR (3200, 3699) )
                            ]
    (temp, finalSeed) = tempGen nextSeed
  