{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

import Apecs
import Control.Monad
import qualified Data.Set as Set

import Flow
import GHC.Generics (Generic)
import Graphics.Gloss
import Linear (V2 (..), V3(..))
import qualified Data.Map.Strict as Map
import System.Random (Random, RandomGen, StdGen)
import qualified System.Random
import Units.Length (Length)
import qualified Units.Length as Length
import Units.Percent (Percent)
import qualified Units.Percent as Percent
import Units.Temperature (Temperature)
import qualified Random
import qualified Data.Star as Star
import Population (Population)
import qualified Population
import Data.Knowledge (Knowledge)
import qualified Data.Knowledge as Knowledge
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


newtype Planet = Planet PlanetType
  deriving (Show)

data PlanetType = Rocky | Gas
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Random PlanetType where
  random g = case System.Random.randomR (0,2) g of
                 (r, g') -> (toEnum r, g')
  randomR (a,b) g = case System.Random.randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')

data Water = Water
  deriving (Show)


type WaterPercent = Percent Water


newtype Orbit = Orbit Int
  deriving (Show)


newtype Size = Size Float
  deriving (Show)


newtype Parent = Parent Entity
  deriving (Show)


newtype CivPopulation = CivPopulation (Map.Map Entity Population)
  deriving (Show)


newtype Named = Named String
  deriving (Show)


newtype ReproductionRate = ReproductionRate Float
  deriving (Show)


newtype MortalityRate = MortalityRate Float
  deriving (Show)


newtype CivHappniness = CivHappniness (Map.Map Entity Float)
  deriving (Show)


newtype CivKnowledge = CivKnowledge Knowledge
  deriving (Show)


makeWorldAndComponents "World"
  [ ''StdGen
  , ''SolarSystem
  , ''Star
  , ''Planet
  , ''WaterPercent
  , ''Orbit
  , ''Size
  , ''Parent
  , ''CivPopulation
  , ''Named
  , ''ReproductionRate
  , ''MortalityRate
  , ''CivHappniness
  , ''CivKnowledge
  ]


type System' a = System World a 


game :: System' ()
game = do
  randSeed <- liftIO System.Random.initStdGen
  let (solarSystemCount, nextSeed) = Random.range randSeed 3 8
  global $= nextSeed
  replicateM solarSystemCount randomSolarSystem


  cmapM_ $ \(Planet, Entity e)
  let allKnowledge = Knowledge.buildKnowledgeTree
                      List.concatMap
                        (\planet ->
                          List.concat
                              [ ---- Local solar knowledge
                                fmap
                                  (\sibling -> do
                                    isStar <- exists sibling (Proxy @(Star))
                                    pure <|
                                      if isStarthen
                                        -- No previous knowledge required
                                        ( KnowsOf sibling, [] )
                                      else
                                        -- Requires Optics
                                        ( KnowsOf sibling, [ Set.singleton Optics ] )
                                  )
                                  (Set.toList solarSiblings)
                              ---- Galactic knowledge
                              -- Knowledge of extra solar stars only requires Optics
                              , List.map
                                  (\nonSiblingStarId ->
                                      ( KnowsOf nonSiblingStarId, [ Set.Any.singleton Data.Knowledge.comparableConfig Optics ] )
                                  )
                                  (Set.toList (Set.difference worldWithPlayerCiv.stars solarSiblings))

                              -- Knowledge of extra solar planets requires Optics and knowledge of at least 1 parent local star
                              , List.map
                                  (\nonSiblingPlanetId ->
                                      ( KnowsOf nonSiblingPlanetId
                                      , List.map
                                          (\foreignStarId ->
                                              Set.Any.fromList Data.Knowledge.comparableConfig
                                                  [ Optics, KnowsOf foreignStarId ]
                                          )
                                          (Set.toList (Set.intersect worldWithPlayerCiv.stars (getSolarSiblings worldWithPlayerCiv nonSiblingPlanetId)))
                                      )
                                  )
                                  (Set.toList (Set.diff worldWithPlayerCiv.planets solarSiblings))

                              ---- Other Civ Knowledge
                              ]
                              where
                                solarSiblings = getSolarSiblings planet
                        )
                        (Set.toList worldWithPlayerCiv.planets)
                       
  -- Debug display data
  liftIO <| putStrLn "\nSolar System positions:"
  cmapM_ $ \(SolarSystem _ _ pos, Entity _) -> (pos) |> print |> liftIO
  liftIO <| putStrLn "\nStar temps:"
  cmapM_ $ \(Star temp, Entity _) -> (temp) |> print |> liftIO
  liftIO <| putStrLn "\nCivs:"
  cmapM_ $ \(Named name, CivPopulation pop, Entity _) -> (name, pop) |> print |> liftIO


getSolarSiblings :: Entity -> System' (Set.Set Entity)
getSolarSiblings child = do
    (Parent p) <- get child
    (SolarSystem { stars, planets }) <- get p
    Set.union stars planets |> pure
  

randomSolarSystem :: System' Entity
randomSolarSystem = do
  randSeed <- get global
  let (starCount, starSeed) = Random.weighted randSeed ( 56.0, 1 ) [ ( 33.0, 2 ), ( 8.0, 3 ), ( 1.0, 4 ), ( 1.0, 5 ), ( 1.0, 6 ), ( 1.0, 7 ) ]
  global $= (starSeed :: StdGen)
  stars <- replicateM starCount randomStar
  nextRandSeed <- get global
  let (planetCount, planetSeed) = Random.range nextRandSeed 1 12
  global $= (planetSeed :: StdGen)
  planets <- replicateM planetCount randomPlanet

  position <- randomGalacticPosition

  solarSystem <- newEntity
                  (SolarSystem
                    { stars = Set.fromList stars
                    , planets = Set.fromList planets
                    , position = position
                    }
                  )

  cmapM $ \(Planet _, e) -> e $= Parent solarSystem
  cmapM $ \(Star _, e) -> e $= Parent solarSystem

  pure solarSystem


randomGalacticPosition :: System' (V3 Length)
randomGalacticPosition = do
  randSeed <- get global
  let (rand1, nextSeed) = Random.range randSeed 0 1.0
  let (rand2, finalSeed) = Random.range nextSeed 0 1.0
  global $= (finalSeed :: StdGen)
  
  let radius = (Length.lightYears 50000 |> Length.inMeters)
  let u1 = 2 * pi * rand1
  let u2 = radius * rand2

  pure <| V3 (Length.meters (u2 * cos u1)) (Length.meters (u2 * sin u1)) (Length.meters 0)


randomStar :: System' Entity
randomStar = do
  (randSeed :: StdGen) <- get global
  let (temp, nextSeed) = Star.generate randSeed
  global $= (nextSeed :: StdGen)
  newEntity <| Star temp


randomPlanet :: System' Entity
randomPlanet = do
  randSeed <- get global
  
  let (planetType, typeSeed) = Random.range randSeed Rocky Gas
  let (waterPercent, waterSeed) = Random.range typeSeed 0.0 100.0

  let (orbit, orbitSeed) = planetOrbit waterSeed planetType
  let (size, sizeSeed) = planetSize orbitSeed planetType
  
  global $= (sizeSeed :: StdGen)
  
  planet <- newEntity
              ( Planet planetType
              , Percent.fromFloat waterPercent :: WaterPercent
              , Orbit orbit
              , Size size
              )

  attemptToGenerateCivilization planetType planet
  
  pure planet


planetOrbit :: (RandomGen g) => g -> PlanetType -> (Int, g)
planetOrbit seed planetType =
  case planetType of
    Rocky -> Random.range seed 0 7
    Gas -> Random.range seed 5 12


planetSize :: (RandomGen g) => g -> PlanetType -> (Float, g)
planetSize seed planetType =
  case planetType of
    Rocky -> Random.range seed 1000.0 8000.0
    Gas -> Random.range seed 22000.0 90000.0


attemptToGenerateCivilization :: PlanetType -> Entity -> System' ()
attemptToGenerateCivilization planetType planet =
  if planetType == Rocky
    then do
      randSeed <- get global
      let (shouldCreateCiv,genSeed) = Random.oneIn randSeed 10
      global $= (genSeed :: StdGen)

      if shouldCreateCiv
        then do
          nextRandSeed <- get global
          let (maybeName, _, nameSeed) = Random.choose nextRandSeed allCivNames
          global $= (nameSeed :: StdGen)
          case maybeName of
            Nothing -> pure ()
            Just name -> generateCivilization planet name
        else pure ()
    else pure ()


allCivNames =
    [ "Morlock"
    , "Klingon"
    , "Federation"
    , "Borg"
    , "Empire"
    , "Gorn"
    , "Talonite"
    , "Sha' Tao"
    ]


generateCivilization :: Entity -> String -> System' ()
generateCivilization planet name = do
    -- Random.map4
    --     (\initialPopulationSize reproductionRate mortalityRate initialHappiness ->
    --         let
    --             ( civId, worldWithNewCiv ) =
    --                 Logic.Entity.Extra.create worldWithFewerNames
    --                     |> Logic.Entity.with
    --                         ( Game.Components.civilizationPopulationSpec
    --                         , Dict.singleton planetId (Population.millions initialPopulationSize)
    --                         )
    --                     |> Logic.Entity.with ( Game.Components.namedSpec, name )
    --                     |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, reproductionRate )
    --                     |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, mortalityRate )
    --                     |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
    --                     |> Logic.Entity.with ( Game.Components.knowledgeSpec, Set.Any.empty )
    --         in
    --         { worldWithNewCiv | civilizations = Set.insert civId worldWithNewCiv.civilizations }
    --     )
    --     (Random.float 3 10)
    --     (Rate.random 0.2 0.3)
    --     (Rate.random 0.1 0.2)
    --     (Percent.random 90.0 100.0)
  randSeed <- get global
  let (populationSize, popSeed) = Random.range randSeed 3 10
  let (reproductionRate, repoSeed) = Random.range popSeed 0.2 0.3
  let (mortalityRate, mortalitySeed) = Random.range repoSeed 0.1 0.2
  let (happiness, happySeed) = Random.range mortalitySeed 90.0 100.0
  global $= (happySeed :: StdGen)

  newEntity_
    ( CivPopulation (Map.singleton planet (Population.millions populationSize))
    , Named name
    , ReproductionRate reproductionRate
    , MortalityRate mortalityRate
    , CivHappniness (Map.singleton planet happiness)
    -- , Knowledge TODO
    )

  pure ()


main :: IO ()
main = do
  w <- initWorld
  runSystem game w
  -- display (InWindow "Space Sim" (800, 600) (10, 10)) white (Circle 80)
