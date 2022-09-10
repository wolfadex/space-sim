module Data.Civilization exposing (CivilizationName, allNames, enhancedEventDescription)

import Data.Names exposing (PersonName)


type alias CivilizationName =
    { singular : String
    , possessive : Maybe String
    , many : Maybe String
    }


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