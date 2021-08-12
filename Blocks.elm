module Blocks exposing (Matrix, remove, select)

import List
import Set exposing (Set)


{-| Matrix is a list the columns (of "a") not rows
-}
type alias Matrix a =
    List (List a)


at : Int -> Int -> Matrix a -> Maybe a
at x y =
    Maybe.andThen (nth y) << nth x


nth : Int -> List a -> Maybe a
nth n =
    List.head << List.drop n


select : ( Int, Int ) -> Matrix a -> Set ( Int, Int )
select ( x, y ) m =
    case at x y m of
        Nothing ->
            Set.empty

        Just example ->
            reachableFrom ( x, y ) <| allSameAs example m


allSameAs : a -> Matrix a -> Set ( Int, Int )
allSameAs example =
    Set.fromList
        << List.filterMap identity
        << List.concat
        << List.indexedMap
            (\x ->
                List.indexedMap <|
                    \y cell ->
                        if cell == example then
                            Just ( x, y )

                        else
                            Nothing
            )


reachableFrom : ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
reachableFrom start =
    let
        go selected rest =
            let
                expanded =
                    expandWith neibs selected

                new =
                    Set.intersect expanded rest
            in
            if Set.isEmpty new then
                selected

            else
                go (Set.union selected new) (Set.diff rest new)
    in
    go <| Set.singleton start


expandWith : (comparable -> List comparable) -> Set comparable -> Set comparable
expandWith fork =
    Set.fromList << List.concatMap fork << Set.toList


neibs : ( Int, Int ) -> List ( Int, Int )
neibs ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


remove : Set ( Int, Int ) -> Matrix a -> Matrix a
remove selected =
    List.filterMap
        (\l ->
            if List.isEmpty l then
                Nothing

            else
                Just l
        )
        << List.indexedMap
            (\x ->
                List.filterMap identity
                    << List.indexedMap
                        (\y cell ->
                            if Set.member ( x, y ) selected then
                                Nothing

                            else
                                Just cell
                        )
            )
