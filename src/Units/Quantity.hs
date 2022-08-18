{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Units.Quantity
  ( Quantity(..)
  , sum
  , difference
  , scaleBy
  ) where

import Prelude hiding (sum)


newtype Quantity unit = Quantity Float
  deriving (Show)


sum :: Quantity unit -> Quantity unit -> Quantity unit
sum (Quantity a) (Quantity b) =
  Quantity (a + b)


difference :: Quantity unit -> Quantity unit -> Quantity unit
difference (Quantity a) (Quantity b) =
  Quantity (a - b)


scaleBy :: Float -> Quantity unit -> Quantity unit
scaleBy a (Quantity qty) =
  Quantity (a * qty)