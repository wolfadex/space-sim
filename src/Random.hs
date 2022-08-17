module Random
    ( weighted
    , range
    ) where

import Flow
import System.Random (Random, RandomGen)
import qualified System.Random



weighted :: RandomGen g => g -> ( Float, a ) -> [( Float, a )] ->  (a, g)
weighted randSeed first others =
  (getByWeight first others cntDwn, finalSeed)
  where
    normalize ( weight, _ ) = abs weight
    total = normalize first + sum (fmap normalize others)
    (cntDwn, finalSeed) = System.Random.randomR (0.0, total) randSeed
    getByWeight ( weight, value ) oths countdown =
      case oths of
        [] -> value
        next : rest ->
          if countdown <= abs weight
            then value
            else getByWeight next rest (countdown - abs weight)

range :: ( RandomGen g, Random a ) => g -> a -> a -> ( a, g )
range randSeed start end =
    System.Random.randomR ( start, end ) randSeed