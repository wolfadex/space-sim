module Markov.String exposing
    ( MarkovString
    , empty
    , train, trainList
    , encode, decode
    , comparableConfig
    )

{-| Train a markov model for string generation. This module provides some wrapper functionality around the markov
module so that you can do word generation. Sentence generation using markov models is fairly straight forward by using
lists of `String`s as training data. Just like how the core `String` module makes some character operations easier this
section helps to wrap `List Char` operations into a more simpler `String` interface. This module contains only the
operations that benefit from simplification not covered by the core library. This sections also serves as a good
template on how to extend the markov package for use in your own data.


# Types

@docs MarkovString


# Builders

@docs empty


# Modifiers

@docs train, trainList


# Json

@docs encode, decode

-}

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Markov exposing (Element(..), Markov)


{-| A type alias for a markov graph which is used on determining probabilities of character transitions in words.
-}
type alias MarkovString =
    Markov Int Char


charComparable : Element Char -> Int
charComparable element =
    case element of
        Start ->
            -2

        Element c ->
            Char.toCode c

        End ->
            -1


{-| Create an empty markov string object.
-}
empty : MarkovString
empty =
    Markov.empty


{-| Train the markov model on a single string. This trains the model to store the transition probabilities of all the
letters within the string as well as the probabilities to start with the first character and end with the last
character.
-}
train : String -> MarkovString -> MarkovString
train string markov =
    Markov.train comparableConfig (String.toList string) markov


comparableConfig :
    { toComparable : Element Char -> Int
    , fromComparable : Int -> Element Char
    }
comparableConfig =
    { toComparable = charComparable
    , fromComparable = \code -> Element (Char.fromCode code)
    }


{-| Train the model on a sample of data or an entire corpus.
-}
trainList : List String -> MarkovString -> MarkovString
trainList strings markov =
    List.foldl train markov strings



-- Json


{-| Encode the markov string model into a json object.
-}
encode : MarkovString -> Value
encode =
    Markov.encode comparableConfig String.fromChar


{-| Decode a json object into a markov string model.
-}
decode : Decoder MarkovString
decode =
    let
        stringToChar : String -> Char
        stringToChar string =
            Maybe.withDefault (Char.fromCode 0) (List.head (String.toList string))
    in
    Markov.decode comparableConfig stringToChar
