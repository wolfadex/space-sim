module Set.Any exposing (AnySet, fromList, member, insert)

import Set exposing (Set)


type AnySet comparable a
    = AnySet (Set comparable)


fromList : { r | toComparable : a -> comparable } -> List a -> AnySet comparable a
fromList config list =
    AnySet (Set.fromList (List.map config.toComparable list))

member :  { r | toComparable : a -> comparable } -> a -> AnySet comparable a -> Bool
member config key (AnySet set) =
    Set.member (config.toComparable key) set



insert : { r | toComparable : a -> comparable } -> a -> AnySet comparable a -> AnySet comparable a
insert config key (AnySet set) =
    AnySet (Set.insert (config.toComparable key) set) 