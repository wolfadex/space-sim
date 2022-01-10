module IdSet exposing (IdSet(..), fromList, toList)

import Id exposing (Id)
import Set exposing (Set)


type IdSet
    = IdSet (Set String)


fromList : List (Id value) -> IdSet
fromList items =
    IdSet (Set.fromList (List.map Id.toString items))


toList : IdSet -> List (Id value)
toList (IdSet set) =
    List.filterMap Id.fromString (Set.toList set)
