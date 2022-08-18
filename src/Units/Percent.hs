{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Units.Percent
  ( Percent
  , fromFloat
  , toFloat
  , zero
  ) where

import Units.Quantity (Quantity(..))


type Percent a = Quantity a


-- 100.0 is equivalent to 100%
fromFloat :: Float -> Percent a
fromFloat = Quantity


toFloat :: Percent a -> Float
toFloat (Quantity a) = a


zero :: Percent a
zero =
    Quantity 0.0