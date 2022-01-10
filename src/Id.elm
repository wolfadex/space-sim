module Id exposing
    ( Id
    , fromString, generate, toString
    )

{-| An `Id` type that leverages the type system (thanks to [phantom types](https://wiki.haskell.org/Phantom_type))
to ensure that it refers to the right value.
For example, if your application handles users with integer-based IDs, and articles with string-based IDs,
you could define them as such:
type User
= User
{ id : UserID
, name : String
}
type Article
= Article
{ id : ArticleID
, title : String
}
type alias UserID =
Id User
type alias ArticleID =
Id Article


# The Id type

@docs Id


# Conversions

@docs from, to

-}

import Random exposing (Generator)
import Uuid exposing (Uuid)


{-| The `Id a value` type handles IDs whose representation are of type `a`
that refers to values of type `value`.
-}
type Id value
    = Id Uuid


{-| Make an `Id` from its representation.
userID : Id String User
userID =
Id.from "e4edf8a"
articleID : Id Int Article
articleID =
Id.from 5
-}
fromString : String -> Maybe (Id value)
fromString uuid =
    Maybe.map Id
        (Uuid.fromString uuid)


{-| Extract the raw representation from an `Id`.
Id.to userID == "e4edf8a"
Id.to articleID == 5
-}
toString : Id value -> String
toString (Id uuid) =
    Uuid.toString uuid


generate : Generator (Id value)
generate =
    Random.map Id
        Uuid.uuidGenerator
