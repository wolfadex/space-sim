module Effect exposing
    ( Effect, none
    , toCmd
    )

{-|

@docs Effect, none
@docs toCmd

-}


type Effect msg
    = None


none : Effect msg
none =
    None



-- Used by Main.elm


toCmd : (pageMsg -> msg) -> Effect pageMsg -> Cmd msg
toCmd _ _ =
    Cmd.none
