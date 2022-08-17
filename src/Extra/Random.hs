module Extra.Random
    ( weighted
    ) where

import Control.Monad.IO.Class (MonadIO)
import Flow
import System.Random (Random)
import qualified System.Random as Random


weighted :: ( Random a, MonadIO m ) => ( Float, a ) -> [( Float, a )] -> m a
weighted first others = do
  let normalize ( weight, _ ) = abs weight
  let total = normalize first + sum (fmap normalize others)

  cntDwn <- Random.randomRIO (0.0, total)

  let getByWeight ( weight, value ) oths countdown =
        case oths of
          [] -> value
          next : rest ->
            if countdown <= abs weight
              then value
              else getByWeight next rest (countdown - abs weight)

  pure <| getByWeight first others cntDwn