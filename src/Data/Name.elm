module Data.Name exposing
    ( PersonName
    , randomPerson
    )

import Random exposing (Generator)


type alias PersonName =
    String


randomPerson : Generator PersonName
randomPerson =
    Random.uniform "Wolfgang" allPersonNames


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
    , "Ryan"
    ]
