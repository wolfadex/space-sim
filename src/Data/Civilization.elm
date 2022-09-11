module Data.Civilization exposing
    ( Characteristics
    , CivilizationName
    , allNames
    , enhancedEventDescription
    , styleSpec
    )

import Data.EarthYear exposing (EarthYear)
import Data.Name exposing (PersonName)
import Logic.Component exposing (Spec)


type alias CivilizationName =
    { singular : String
    , possessive : Maybe String
    , many : Maybe String
    }


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


allNames : List CivilizationName
allNames =
    [ { singular = "Morlock"
      , possessive = Just "Morlock's"
      , many = Just "Morlocks"
      }
    , { singular = "Klingon"
      , possessive = Just "Klingon's"
      , many = Just "Klingons"
      }
    , { singular = "Federation"
      , possessive = Just "Federation's"
      , many = Nothing
      }
    , { singular = "Borg"
      , possessive = Just "Borg's"
      , many = Nothing
      }
    , { singular = "Empire"
      , possessive = Just "Empire's"
      , many = Nothing
      }
    , { singular = "Gorn"
      , possessive = Nothing
      , many = Nothing
      }
    , { singular = "Talonite"
      , possessive = Just "Talonite's"
      , many = Nothing
      }
    , { singular = "Sha' Tao"
      , possessive = Just "Sha' Taolo"
      , many = Nothing
      }
    ]


enhancedEventDescription : CivilizationName -> (PersonName -> String)
enhancedEventDescription civName =
    case civName.singular of
        "Morlock" ->
            \personName ->
                "High Archbrain " ++ personName ++ " of the Morlocks"

        "Gorn" ->
            \_ ->
                "Gorn of the Gorn"

        "Borg" ->
            \_ ->
                "The Collective"

        "Empire" ->
            \personName -> personName ++ " of the Empire"

        "Federation" ->
            \personName -> "Science officer " ++ personName ++ " of the Federation"

        "Klingon" ->
            \personName -> personName ++ " of the Klingon Empire"

        "Talonite" ->
            \personName -> "Lead " ++ personName ++ " of the United Talonite"

        "Sha' Tao" ->
            \personName -> "Most Respected " ++ personName ++ " of the Ancient Sha' Tao"

        _ ->
            identity
