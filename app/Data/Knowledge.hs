module Data.Knowledge
    ( Knowledge(..)
    , KnowledgeTree
    , buildKnowledgeTree
    , canBeLearned
    , knows
    ) where

import Apecs (Entity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data Knowledge
  = LandTravel
  | WaterSurfaceTravel
  | UnderwaterTravel
  | Flight
  | PlanetarySpaceTravel
  | InterplanetarySpaceTravel
  | FTLSpaceTravel
  -- Basics of civilization
  | BasicAgriculture
  | BasicMetalWorking
  | Optics
  -- Things in the universe
  | KnowsOf Entity
  deriving (Show, Eq, Ord)


knows :: Set.Set Knowledge -> Knowledge -> Bool
knows known knowledge = Set.member knowledge known


type KnowledgeTree = Map.Map Knowledge [Set.Set Knowledge]


canBeLearned :: KnowledgeTree -> Set.Set Knowledge -> Set.Set Knowledge
canBeLearned knowledgeTree known =
  Set.fromList
    (Map.keys
      (Map.filter
        (\requirements ->
          case requirements of
            [] -> True
            _ ->
              List.any
                (\setOfRequirements ->
                  Set.null (Set.difference setOfRequirements known)
                )
                requirements
        )
        knowledgeTree
      )
    )


buildKnowledgeTree :: [( Knowledge, [Set.Set Knowledge] )] -> KnowledgeTree
buildKnowledgeTree generatedKnowledge =
  Map.fromList
    -- Fixed knowledge
    ([ ( LandTravel, [] )
      , ( WaterSurfaceTravel, [] )
      , ( UnderwaterTravel, [] )
      , ( BasicAgriculture, [] )
      , ( BasicMetalWorking, [] )
      , ( Optics, [ Set.singleton BasicMetalWorking ] )
      , ( Flight , [ Set.singleton LandTravel, Set.singleton WaterSurfaceTravel ] )
      , ( PlanetarySpaceTravel, [ Set.singleton Flight ] )
      , ( InterplanetarySpaceTravel, [ Set.singleton PlanetarySpaceTravel ] )
      , ( InterplanetarySpaceTravel
        , [ Set.fromList
            [ PlanetarySpaceTravel
            , BasicAgriculture
            , Optics
            ]
          ]
        )
      , ( FTLSpaceTravel, [ Set.singleton InterplanetarySpaceTravel ] )
      ]
      <> generatedKnowledge
      
    )
        