module Data.Civilization exposing
    ( Characteristics
    , styleSpec
    )

import Data.EarthYear exposing (EarthYear)
import Logic.Component exposing (Spec)


{-| Characteristics of a civilization

cooperationVsCompetition:
Whether the civ lean towards copperation or competition. 0.0 being 100% cooperative, like The Borg, and 1.0 being 100% competitive like The Gorn

TODO: soical structure (hierarchy, flat, classes, etc)
TODO: government, governance (democracy, police state, etc)
TODO: religion
TODO: language/communication (written, sound/spoken, smell, taste, signed, etc)
TODO: culture: things like sculptures, music, literature

(separate from this)
TODO: trade

-}
type alias Characteristics =
    { cooperationVsCompetition : Float
    , timeSinceLastMonument : EarthYear
    }


styleSpec : Spec Characteristics { world | civilizationStyle : Logic.Component.Set Characteristics }
styleSpec =
    Logic.Component.Spec .civilizationStyle (\comps world -> { world | civilizationStyle = comps })
