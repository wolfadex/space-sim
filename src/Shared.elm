module Shared exposing (Effect(..), Flags)

import Game.Components exposing (Name)
import Random exposing (Seed)


type alias Flags =
    { seed0 : Int }


type Effect
    = CreateGame { name : Name, homePlanetName : String, seed : Seed }
    | DeleteGame Seed
