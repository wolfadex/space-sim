{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Units.Quantity
  ( Quantity(..)
  , sum
  , scaleBy
  ) where

import Prelude hiding (sum)


newtype Num qty => Quantity qty unit = Quantity qty
  deriving (Show)


sum :: Num qty => Quantity qty unit -> Quantity qty unit -> Quantity qty unit
sum (Quantity a) (Quantity b) =
  Quantity (a + b)


scaleBy :: Num qty => qty -> Quantity qty unit -> Quantity qty unit
scaleBy a (Quantity qty) =
  Quantity (a * qty)