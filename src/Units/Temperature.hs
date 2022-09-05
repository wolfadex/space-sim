module Units.Temperature
  ( Temperature
  , kelvins
  ) where

newtype Temperature = Temperature Float
  deriving (Show)


kelvins :: Float -> Temperature
kelvins = Temperature