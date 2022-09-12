module Dict.Any exposing
    ( AnyDict
    , empty
    , filter
    , fromList
    , get
    , insert
    , keys
    , toList
    , union
    )

import Dict exposing (Dict)


type AnyDict comparable key value
    = AnyDict (Dict comparable value)


empty : AnyDict comparable key value
empty =
    AnyDict Dict.empty


get : { r | toComparable : key -> comparable } -> key -> AnyDict comparable key value -> Maybe value
get config key (AnyDict dict) =
    Dict.get (config.toComparable key) dict


fromList : { r | toComparable : key -> comparable } -> List ( key, value ) -> AnyDict comparable key value
fromList config list =
    AnyDict (Dict.fromList (List.map (\( k, v ) -> ( config.toComparable k, v )) list))


toList : { r | fromComparable : comparable -> key } -> AnyDict comparable key value -> List ( key, value )
toList config (AnyDict dict) =
    List.map (Tuple.mapFirst config.fromComparable) (Dict.toList dict)


insert : { r | toComparable : key -> comparable } -> key -> value -> AnyDict comparable key value -> AnyDict comparable key value
insert config key value (AnyDict dict) =
    AnyDict (Dict.insert (config.toComparable key) value dict)


keys : { r | fromComparable : comparable -> key } -> AnyDict comparable key value -> List key
keys config (AnyDict dict) =
    List.map config.fromComparable (Dict.keys dict)


filter : { r | fromComparable : comparable -> key } -> (key -> value -> Bool) -> AnyDict comparable key value -> AnyDict comparable key value
filter config predicate (AnyDict dict) =
    AnyDict (Dict.filter (\comp val -> predicate (config.fromComparable comp) val) dict)


union : AnyDict comparable key value -> AnyDict comparable key value -> AnyDict comparable key value
union (AnyDict left) (AnyDict right) =
    AnyDict (Dict.union left right)
