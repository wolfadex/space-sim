module Random
    ( weighted
    , range
    , oneIn
    , choose
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


oneIn :: ( RandomGen g ) => g -> Int -> ( Bool, g )
oneIn seed odds =
  (a == 1, g)
  where
    (a, g) = range seed 1 odds



choose :: ( RandomGen g ) => g -> [a] -> (Maybe a, [a], g)
choose seed list =
  case list of
    [] -> (Nothing, list, seed)
    _ ->
      ( getAtIndex index list
      , take index list <> drop (index + 1) list
      , g
      )
      where
        (index, g) = range seed 0 (length list - 1)



getAtIndex :: Int -> [a] -> Maybe a
getAtIndex index list =
  case drop index list of
    [] -> Nothing
    h : _ -> Just h