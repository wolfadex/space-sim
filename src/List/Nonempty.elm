module List.Nonempty exposing
    ( Nonempty(..)
    , appendList
    , codec
    , head
    , indexedMap
    , length
    , singleton
    , tail
    , toList
    )

import Serialize


type Nonempty a
    = Nonempty ( a, List a )


singleton : a -> Nonempty a
singleton a =
    Nonempty ( a, [] )


head : Nonempty a -> a
head (Nonempty ( a, _ )) =
    a


tail : Nonempty a -> List a
tail (Nonempty ( _, list )) =
    list


indexedMap : (Int -> a -> b) -> Nonempty a -> Nonempty b
indexedMap fn (Nonempty ( a, list )) =
    Nonempty ( fn 0 a, List.indexedMap (\i b -> fn (i + 1) b) list )


appendList : List a -> Nonempty a -> Nonempty a
appendList new (Nonempty ( a, old )) =
    Nonempty ( a, old ++ new )


toList : Nonempty a -> List a
toList (Nonempty ( a, list )) =
    a :: list


length : Nonempty a -> Int
length (Nonempty ( _, list )) =
    List.length list + 1


codec : Serialize.Codec e a -> Serialize.Codec e (Nonempty a)
codec valueCodec =
    Serialize.record (\first rest -> Nonempty ( first, rest ))
        |> Serialize.field head valueCodec
        |> Serialize.field tail (Serialize.list valueCodec)
        |> Serialize.finishRecord
