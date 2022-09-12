module Data.Name exposing
    ( PersonName
    , randomPerson
    )

import Markov
import Markov.String
import Random exposing (Generator)


type alias PersonName =
    String


randomPerson : Generator PersonName
randomPerson =
    -- Random.uniform "Wolfgang" allPersonNames
    Random.map (\chars -> String.dropRight 1 (String.fromList chars))
        (Markov.generateSequence
            Markov.String.comparableConfig
            { maxLength = 15 }
            (Markov.String.trainList allPersonNames Markov.empty)
        )


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
