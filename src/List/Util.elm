module List.Util exposing (groupsOfTwo)


groupsOfTwo : List a -> List ( a, a )
groupsOfTwo list =
    Tuple.first
        (List.foldr
            (\a ( acc, maybeLast ) ->
                case maybeLast of
                    Just last ->
                        ( ( a, last ) :: acc, Just a )

                    Nothing ->
                        ( acc, Just a )
            )
            ( [], Nothing )
            list
        )
