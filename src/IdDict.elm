module IdDict exposing
    ( IdDict
    , empty
    , fromList
    , get
    , keys
    , singleton
    , toList
    , union
    )

import Dict exposing (Dict)
import Id exposing (Id)


type IdDict value
    = IdDict (Dict String value)


empty : IdDict value
empty =
    IdDict Dict.empty


union : IdDict value -> IdDict value -> IdDict value
union (IdDict left) (IdDict right) =
    IdDict (Dict.union left right)


keys : IdDict value -> List (Id value)
keys (IdDict dict) =
    List.filterMap Id.fromString
        (Dict.keys dict)


singleton : Id value -> value -> IdDict value
singleton key value =
    IdDict (Dict.singleton (Id.toString key) value)


fromList : List ( Id value, value ) -> IdDict value
fromList items =
    IdDict
        (Dict.fromList
            (List.map
                (\( id, value ) ->
                    ( Id.toString id
                    , value
                    )
                )
                items
            )
        )


toList : IdDict value -> List ( Id resource, value )
toList (IdDict dict) =
    List.filterMap
        (\( idStr, value ) ->
            Maybe.map (\id -> ( id, value ))
                (Id.fromString idStr)
        )
        (Dict.toList dict)


get : Id value -> IdDict a -> Maybe a
get key (IdDict dict) =
    Dict.get (Id.toString key) dict
