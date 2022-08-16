{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Units.Length
  ( Length
  , Meters(..)
  , meters
  , inMeters
  , lightYears
  ) where

import Flow

import qualified Units.Constants as Constants
import Units.Quantity (Quantity(..))
import qualified Units.Quantity as Quantity

data Meters = Meters
  deriving (Show)


type Length = Quantity Float Meters


meters :: Float -> Length
meters = Quantity


inMeters :: Length -> Float
inMeters (Quantity m) =
  m


lightYears :: Float -> Length
lightYears numLightYears =
  numLightYears * Constants.lightYear
    |> meters