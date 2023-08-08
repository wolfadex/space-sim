module Data.Civilization exposing
    ( Characteristics
    , Sense(..)
    , allSenses
    , senseComparableConfig
    , styleSpec
    )

import Data.EarthYear exposing (EarthYear)
import Dict exposing (Dict)
import Logic.Component exposing (Spec)
import Set.Any exposing (AnySet)


{-| Characteristics of a civilization

cooperationVsCompetition:
Whether the civ lean towards copperation or competition. 0.0 being 100% cooperative, like Borg, and 1.0 being 100% competitive like Gorn

timeSinceLastMonument:
When the strucutre was built that will last a long time and could be "discovered" by a future civilization, or recognized as "important"/"influential"

senses:
How the civilization communicates. Effects how, or whether, civilizations are able to communicate with each other

descisionMakingStructure:
The social & governmental approach of the civ.
Distributed <---> Top down
0.0 <---> 1.0
Fungi <---> Borg, Monarchy

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
    , senses : AnySet Int Sense
    , descisionMakingStructure : Float
    }


styleSpec : Spec Characteristics { world | civilizationStyle : Logic.Component.Set Characteristics }
styleSpec =
    Logic.Component.Spec .civilizationStyle (\comps world -> { world | civilizationStyle = comps })



-- type
--     Government
--     -- Run by groups, paritally distributed, top down
--     = Democracy
--       -- Top down, balance
--     | Communism
--       -- partially distributed
--     | Socialism
--       -- top down
--     | Oligarchy
--       -- top down, small groups, money driven
--     | Aristocracy
--       -- top down, minority ruling
--     | Monarchy
--       -- top down, small groups, ideology driven
--     | Theocracy
--       -- ruling group, subservent group
--     | Colonialism
--     | Totalitarianism
--       -- top down, small groups, power driven
--     | Dictatorship


type Sense
    = Visual
    | Audio
    | Pressure -- touch
    | Smell -- Chemical, smell/taste
    | Telepathic -- Other waves, could also cover something like Borg style communication. Telepathic is more how it appears to humans


allSenses : List Sense
allSenses =
    [ Visual
    , Audio
    , Pressure
    , Smell
    , Telepathic
    ]


senseComparableConfig :
    { toComparable : Sense -> Int
    , fromComparable : Int -> Sense
    }
senseComparableConfig =
    { fromComparable =
        \i ->
            case i of
                0 ->
                    Visual

                1 ->
                    Audio

                2 ->
                    Pressure

                3 ->
                    Smell

                4 ->
                    Telepathic

                _ ->
                    Visual
    , toComparable =
        \sense ->
            case sense of
                Visual ->
                    0

                Audio ->
                    1

                Pressure ->
                    2

                Smell ->
                    3

                Telepathic ->
                    4
    }



{- Inspiration

   - parasites (Stargate Goa’uld)
   - machines (Stargate Replicators)
   - Star Trek Gorn
   - Star Trek Q
   - Star Trek Jem'Hadar
   - Star Trek Trill
   - Star Trek Borg
   - Star Trek Hirogen
   - Star Trek species 10-C
   - native to water
   - native to land
   - native to air/flying/floating

-}
-- type Makeup
--     = OrganicCarbon
--     | Silicon
--     | Energy
-- type Size
--     = Tiny
--     | Small
--     | Medium
--     | Large
--     | Giant
--     | Gargantuan
