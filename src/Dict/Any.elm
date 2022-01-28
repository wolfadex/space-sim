module Dict.Any exposing
    ( AnyDict
    , filter
    , fromList
    , keys
    )

import Dict exposing (Dict)


type AnyDict comparable key value
    = AnyDict (Dict comparable value)


fromList : { r | toComparable : key -> comparable } -> List ( key, value ) -> AnyDict comparable key value
fromList config list =
    AnyDict (Dict.fromList (List.map (\( k, v ) -> ( config.toComparable k, v )) list))


keys : { r | fromComparable : comparable -> key } -> AnyDict comparable key value -> List key
keys config (AnyDict dict) =
    List.map config.fromComparable (Dict.keys dict)


filter : { r | fromComparable : comparable -> key } -> (key -> value -> Bool) -> AnyDict comparable key value -> AnyDict comparable key value
filter config predicate (AnyDict dict) =
    AnyDict (Dict.filter (\comp val -> predicate (config.fromComparable comp) val) dict)
