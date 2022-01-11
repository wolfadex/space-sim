module Logic.Entity.Extra exposing
    ( Internals
    , create
    , initInternals
    )

import Logic.Entity exposing (EntityID)
import Set exposing (Set)


type alias Internals =
    ( Set EntityID, EntityID )


create : { world | ecsInternals : Internals } -> ( EntityID, { world | ecsInternals : Internals } )
create world =
    let
        ( id, internals ) =
            getNextId world.ecsInternals
    in
    Logic.Entity.create id { world | ecsInternals = internals }


getNextId : Internals -> ( EntityID, Internals )
getNextId ( availabelOldIds, nextNewId ) =
    case Set.toList availabelOldIds of
        [] ->
            ( nextNewId, ( availabelOldIds, nextNewId + 1 ) )

        nextId :: keepIds ->
            ( nextId, ( Set.fromList keepIds, nextNewId ) )


initInternals : Internals
initInternals =
    ( Set.empty, 0 )
