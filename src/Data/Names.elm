module Data.Names exposing
    ( ComplexName
    , SimleName
    , allCivilizationNames
    , allPersonNames
    )


type alias ComplexName =
    { singular : String
    , possessive : Maybe String
    , many : Maybe String
    }


allCivilizationNames : List ComplexName
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


type alias SimleName =
    String


allPersonNames : List SimleName
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
