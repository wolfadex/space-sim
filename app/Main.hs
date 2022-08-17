{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

import Apecs
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as Set

import Flow
import GHC.Generics (Generic)
import Graphics.Gloss
import Linear (V2 (..), V3(..))
import System.Random (Random)
import qualified System.Random as Random
import Units.Length (Length)
import qualified Units.Length as Length
import qualified Extra.Random
import qualified Data.Star as Star
-- 
import Debug


data SolarSystem = SolarSystem
  { stars :: Set.Set Entity
  , planets :: Set.Set Entity
  , position :: V3 Length
  }
  deriving (Show)


newtype Star = Star Temperature
  deriving (Show)

data Planet = Rocky | Water | Gas
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Random Planet where
  random g = case Random.randomR (0,2) g of
                 (r, g') -> (toEnum r, g')
  randomR (a,b) g = case Random.randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')


makeWorldAndComponents "World" [''SolarSystem, ''Star, ''Planet]


type System' a = System World a 


game :: System' ()
game = do
  solarSystemCount <- Random.randomRIO ( 3, 8) |> liftIO
  replicateM solarSystemCount randomSolarSystem

  cmapM_ $ \(SolarSystem _ _ pos, Entity _) -> (pos) |> print |> liftIO

  cmapM_ $ \(Star temp, Entity _) -> (temp) |> print |> liftIO
  

randomSolarSystem :: System' Entity
randomSolarSystem = do
  starCount <- Extra.Random.weighted ( 56.0, 1 ) [ ( 33.0, 2 ), ( 8.0, 3 ), ( 1.0, 4 ), ( 1.0, 5 ), ( 1.0, 6 ), ( 1.0, 7 ) ]
                |> liftIO
  stars <- replicateM starCount randomStar

  planetCount <- Random.randomRIO ( 3, 8) |> liftIO
  planets <- replicateM planetCount randomPlanet

  position <- randomGalacticPosition

  newEntity
    (SolarSystem
      { stars = Set.fromList stars
      , planets = Set.fromList planets
      , position = position
      }
    )


randomGalacticPosition :: System' (V3 Length)
randomGalacticPosition = do
  rand1 <- Random.randomRIO (0, 1.0) |> liftIO
  rand2 <- Random.randomRIO (0, 1.0) |> liftIO

  let radius = (Length.lightYears 50000 |> Length.inMeters)
  let u1 = 2 * pi * rand1
  let u2 = radius * rand2

  pure <| V3 (Length.meters (u2 * cos u1)) (Length.meters (u2 * sin u1)) (Length.meters 0)


randomStar :: System' Entity
randomStar = do
  temp <- Star.generate |> liftIO
  newEntity <| Star temp


randomPlanet :: System' Entity
randomPlanet = do
  planetType <- Random.randomRIO (Rocky, Gas) |> liftIO
  newEntity planetType


main :: IO ()
main = do
  w <- initWorld
  runSystem game w
  -- display (InWindow "Space Sim" (800, 600) (10, 10)) white (Circle 80)
