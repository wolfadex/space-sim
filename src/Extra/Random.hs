module Extra.Random
    ( weighted
    ) where

import Flow
import System.Random (Random, RandomGen)
import qualified System.Random as Random


weighted :: RandomGen g => g -> ( Float, a ) -> [( Float, a )] ->  (a, g)
weighted randSeed first others =
  (getByWeight first others cntDwn, finalSeed)
  where
    normalize ( weight, _ ) = abs weight
    total = normalize first + sum (fmap normalize others)
    (cntDwn, finalSeed) = Random.randomR (0.0, total) randSeed
    getByWeight ( weight, value ) oths countdown =
      case oths of
        [] -> value
        next : rest ->
          if countdown <= abs weight
            then value
            else getByWeight next rest (countdown - abs weight)

  