module Shared exposing (Effect(..), Flags)

import Data.Names exposing (ComplexName)
import Random exposing (Seed)


type alias Flags =
    { seed0 : Int }


type Effect
    = CreateGame { name : ComplexName, homePlanetName : String, seed : Seed }
    | DeleteGame Seed
