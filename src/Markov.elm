module Markov exposing
    ( Markov, Element(..)
    , empty
    , alphabet, probability, transitionProbabilities
    , add, train, trainList
    , SequenceSettings, generateSequence
    , encode, decode
    )

{-| Copied from <https://github.com/Evelios/elm-markov/tree/1.0.1> to alter the AnyDict implementation

A markov model is a representation internally of transition
probabilities. This can be used for many different applications. The easiest to explain are creating random word
generators (see the `Markov.String` module) and random sentence generators. These are created by training the model
with a particular corpus of material of a particular genre. You can then create new


# Types

@docs Markov, Element


# Builders

@docs empty


# Accessors

@docs alphabet, probability, transitionProbabilities


# Modifiers

@docs add, train, trainList


# Generation

@docs SequenceSettings, generateSequence


# Json

@docs encode, decode

-}

import Dict
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Util
import Random exposing (Generator)


{-| This is the main data structure for the markov models. This data structure internally uses a sparse representation
for transition probabilities. This will be performant when you have many different possible states and they don't all
connect to each other. If you have a lot of transition states and each state can transition to almost every other state,
this implementation will not be as performant as it could be.
-}
type Markov comparable a
    = Markov (AnyDict comparable (Element a) (AnyDict comparable (Element a) Int))


{-| Elements are needed to define the beginning and end of a chain. We don't only need to know the transition
probabilities from one element to another. We need to know the chance that a chain starts with an element, or the chance
that the chain will end with an element.
-}
type Element a
    = Start
    | Element a
    | End



-- Builders


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model. In order to store the transition properties, we need a way of serializing your data.

    charComparable : Element Char -> Int
    charComparable element =
        case element of
            Start ->
                -2

            Element c ->
                Char.toCode c

            End ->
                -1

-}
empty : Markov comparable a
empty =
    Markov Dict.Any.empty



-- Accessors


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : { r | toComparable : Element a -> comparable } -> Element a -> Element a -> Markov comparable a -> Maybe Int
get conversion from to (Markov transitionTable) =
    Maybe.andThen (Dict.Any.get conversion to)
        (Dict.Any.get conversion from transitionTable)


{-| Get the alphabet of the current markov model.
-}
alphabet : { r | fromComparable : comparable -> Element a } -> Markov comparable a -> List a
alphabet conversion (Markov transitionTable) =
    extractElements (Dict.Any.keys conversion transitionTable)


{-| Get the probability of a particular transition state.
-}
probability :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> Element a
    -> Element a
    -> Markov comparable a
    -> Float
probability conversion from to markov =
    case
        List.foldl
            (\( element, eleProb ) _ ->
                if element == to then
                    Just eleProb

                else
                    Nothing
            )
            Nothing
            (transitionProbabilities conversion from markov)
    of
        Nothing ->
            0

        Just f ->
            f


{-| For a particular element, get the probabilities for transitioning from the input element to all the other elements
which are accessible from the input elements. This only returns elements with non-zero probabilities. All probabilities
are normalized to be within the (0 -> 1] range.
-}
transitionProbabilities :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> Element a
    -> Markov comparable a
    -> List ( Element a, Float )
transitionProbabilities conversion from (Markov transitionTable) =
    let
        maybeTransitionCounts : Maybe (List ( Element a, Int ))
        maybeTransitionCounts =
            Maybe.map (Dict.Any.toList conversion)
                (Dict.Any.get conversion from transitionTable)
    in
    case maybeTransitionCounts of
        Just transitionCounts ->
            let
                total : Int
                total =
                    List.sum (List.map Tuple.second transitionCounts)
            in
            List.map
                (\( e, count ) ->
                    ( e, toFloat count / toFloat total )
                )
                transitionCounts

        Nothing ->
            []



-- Modifiers


{-| Add a transition into the markov graph. If the character is not an uppercase or lowercase character or a digit then
the transition is not added.
-}
add :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> Element a
    -> Element a
    -> Markov comparable a
    -> Markov comparable a
add conversion from to ((Markov transitionTable) as markov) =
    let
        set : Int -> Markov comparable a
        set value =
            (\table -> Markov table)
                ((\toTable -> Dict.Any.insert conversion from toTable transitionTable)
                    (Dict.Any.insert conversion
                        to
                        value
                        (Maybe.withDefault
                            Dict.Any.empty
                            (Dict.Any.get conversion from transitionTable)
                        )
                    )
                )
    in
    set
        ((case get conversion from to markov of
            Nothing ->
                0

            Just f ->
                f
         )
            + 1
        )


{-| Add a sequence of transitions. This function adds the Start and End to the list so that you are able to train
the data with starting and ending probabilities as well.
-}
train :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> List a
    -> Markov comparable a
    -> Markov comparable a
train conversion trainingData markov =
    if List.isEmpty trainingData then
        markov

    else
        List.foldl (\( from, to ) -> add conversion from to)
            markov
            (List.Util.groupsOfTwo
                ((\beginning -> List.append beginning [ End ])
                    (Start :: List.map Element trainingData)
                )
            )


{-| Train the markov model on multiple sequences at once.
-}
trainList :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> List (List a)
    -> Markov comparable a
    -> Markov comparable a
trainList conversion trainingLists markov =
    List.foldl (train conversion) markov trainingLists



-- Helper Functions


{-| Private: Extract all of the user objects from a list of markov elements.
-}
extractElements : List (Element a) -> List a
extractElements =
    List.filterMap
        (\e ->
            case e of
                Element a ->
                    Just a

                _ ->
                    Nothing
        )



-- Generate


{-| The parameters used for `generateSequence`. This are setup in record format so that this function can be extended
later.
-}
type alias SequenceSettings =
    { maxLength : Int
    }


{-| Generate a sequence of elements using the probabilities within the markov transition graph. This sequence should
come out to be a random length and takes into account the `Start -> a` and the `a -> End` transition probabilities.
This ensures that the starting and ending weights are taken into account as well.
-}
generateSequence :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> SequenceSettings
    -> Markov comparable a
    -> Generator (List a)
generateSequence conversion settings markov =
    let
        phraseHelper : Int -> Element a -> List (Element a) -> Generator (List (Element a))
        phraseHelper remainingDepth prevElement accumulator =
            -- elm-review: IGNORE TCO
            case remainingDepth of
                0 ->
                    Random.constant accumulator

                _ ->
                    Random.andThen
                        (\nextElement ->
                            case nextElement of
                                Element _ ->
                                    phraseHelper
                                        (remainingDepth - 1)
                                        nextElement
                                        (List.append accumulator [ nextElement ])

                                _ ->
                                    Random.constant accumulator
                        )
                        (genNextElement prevElement)

        genNextElement : Element a -> Generator (Element a)
        genNextElement prevElement =
            let
                probabilities : List ( Float, Element a )
                probabilities =
                    List.map tupleFlip
                        (transitionProbabilities conversion prevElement markov)
            in
            case probabilities of
                [] ->
                    Random.constant End

                firstPossibility :: remainingPossibilities ->
                    Random.weighted firstPossibility remainingPossibilities
    in
    Random.map extractElements
        (phraseHelper settings.maxLength Start [])


tupleFlip : ( a, b ) -> ( b, a )
tupleFlip ( a, b ) =
    ( b, a )



-- Json


{-| Encode a markov graph into a json object. In order to encode a markov graph we need to know how to encode your
object into json data. Unfortunately because of the current implementation, the object needs to be converted into a
string so that it can be used as the json "key" object for storing transition probabilities.
-}
encode : { r | fromComparable : comparable -> Element a } -> (a -> String) -> Markov comparable a -> Value
encode conversion toString (Markov transitionTable) =
    let
        elementToString : Element a -> String
        elementToString element =
            case element of
                Start ->
                    "start"

                End ->
                    "end"

                Element value ->
                    toString value

        encodeRow : AnyDict comparable (Element a) Int -> Value
        encodeRow row =
            -- Dict.Any.encode elementToString Encode.int row
            Encode.object (List.map (Tuple.mapBoth elementToString Encode.int) (Dict.Any.toList conversion row))
    in
    -- Dict.Any.encode elementToString encodeRow transitionTable
    Encode.object
        (List.map (Tuple.mapBoth elementToString encodeRow)
            (Dict.Any.toList conversion transitionTable)
        )


{-| Decode a json object into a markov graph. In order to decode the model, you need to have the inverse of the encoding
function. This function needs to be able to convert the string object you created into your object. It also needs to
take the same function that you used to create the markov graph in the `Markov.empty` function. This is what was used
to store that element into a transition graph.
-}
decode :
    { r
        | toComparable : Element a -> comparable
        , fromComparable : comparable -> Element a
    }
    -> (String -> a)
    -> Decoder (Markov comparable a)
decode conversion decoder =
    let
        stringToElement : String -> Element a
        stringToElement value =
            case value of
                "start" ->
                    Start

                "end" ->
                    End

                _ ->
                    Element (decoder value)

        decodeDict : Decoder b -> Decoder (AnyDict comparable (Element a) b)
        decodeDict =
            decodeHelper conversion (\s _ -> stringToElement s)
    in
    Decode.map Markov (decodeDict (decodeDict Decode.int))


decodeHelper :
    { r | toComparable : Element a -> comparable }
    -> (String -> b -> Element a)
    -> Decoder b
    -> Decoder (AnyDict comparable (Element a) b)
decodeHelper conversion fromStr valueD =
    let
        construct : String -> b -> AnyDict comparable (Element a) b -> AnyDict comparable (Element a) b
        construct strK v acc =
            Dict.Any.insert conversion (fromStr strK v) v acc

        carlA : Decoder (Dict.Dict String b)
        carlA =
            Decode.dict valueD

        carlB : Dict.Dict String b -> AnyDict comparable (Element a) b
        carlB =
            Dict.foldr construct Dict.Any.empty
    in
    Decode.map carlB
        carlA
