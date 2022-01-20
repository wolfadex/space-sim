module Set.Any exposing
    ( AnySet
    , empty
    , insert
    , member
    )

import Set exposing (Set)


type AnySet comparable a
    = AnySet (Set comparable)


member : { r | toComparable : a -> comparable } -> a -> AnySet comparable a -> Bool
member config key (AnySet set) =
    Set.member (config.toComparable key) set


insert : { r | toComparable : a -> comparable } -> a -> AnySet comparable a -> AnySet comparable a
insert config key (AnySet set) =
    AnySet (Set.insert (config.toComparable key) set)


empty : AnySet comparable a
empty =
    AnySet Set.empty
