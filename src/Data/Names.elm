module Data.Names exposing
    ( CivilizationName
    , PersonName
    , allCivilizationNames
    , enhancedEventDescription
    , randomPerson
    )

import Random exposing (Generator)


type alias CivilizationName =
    { singular : String
    , possessive : Maybe String
    , many : Maybe String
    }


allCivilizationNames : List CivilizationName
allCivilizationNames =
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


type alias PersonName =
    String


randomPerson : Generator PersonName
randomPerson =
    Random.weighted ( 0.000001, "Wolfgang" ) (List.map (Tuple.pair 100.0) allPersonNames)


allPersonNames : List PersonName
allPersonNames =
    [ "John"
    , "Klorg"
    , "Bekah"
    , "Stan"
    , "Liesje"
    , "Raechel"
    , "Uriah"
    , "Kelby"
    , "Carey"
    , "Alex"
    , "Berns"
    , "Gwenhidwy"
    , "Xiao"
    , "Guze"
    , "Janiczek"
    , "Kearns"
    , "Jeroen"
    , "Glatz"
    , "Matt"
    ]
