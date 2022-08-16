module Debug (log) where

import Prelude hiding (log)
import qualified Debug.Trace


log :: Show a => String -> a -> a
log msg val =
    Debug.Trace.trace (msg <> ": " <> show val) val