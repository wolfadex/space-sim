module Data.Star
    ( generate
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Extra.Random
import Flow
import System.Random (Random)
import qualified System.Random as Random
import Units.Temperature (Temperature)
import qualified Units.Temperature as Temperature


-- The temperature range is weighted based on abundance, and then a random temp for that abundance's range is generated
generate :: ( MonadIO m ) => m Temperature
generate = do
  tempGen <- Extra.Random.weighted
            ( 0.00001, Random.randomRIO (33000, 42000) )
            [ ( 0.1, Random.randomRIO (10000, 32999) )
            , ( 0.7, Random.randomRIO (7500, 19999) )
            , ( 2.0, Random.randomRIO (6000, 7499) )
            , ( 3.5, Random.randomRIO (5200, 6999) )
            , ( 8.0, Random.randomRIO (3700, 5199) )
            , ( 80.0, Random.randomRIO (3200, 3699) )
            ]
  temp <- liftIO tempGen :: m Float
  pure <| Temperature.kelvins temp