module Shared exposing (Effect(..), Flags)

import Data.Names exposing (CivilizationName)
import Random exposing (Seed)


type alias Flags =
    { seed0 : Int }


type Effect
    = CreateGame { name : CivilizationName, homePlanetName : String, seed : Seed }
    | DeleteGame Seed
