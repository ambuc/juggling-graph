module XVals exposing (mkIns, mkOuts)

import Result


--

import Lib exposing (..)


-- XOUTS


type alias XOuts =
    List ( Coord, JumpDistance )


{-| Returns a sorted list of all throw terminal coordinates.
-}
mkOuts : Opts -> Expr -> List ( Index, Coord, JumpDistance )
mkOuts os expr =
    List.map
        (\( idx, chr ) ->
            ( idx
            , toCoordI os idx + os.h_off
            , toInt chr
            )
        )
    <|
        List.filter (\( _, chr ) -> chr /= ']' && chr /= '[') <|
            List.indexedMap (,) (String.toList expr)



-- XINS


{-| Returns a sorted list of all catch terminal coordinates.
-}
mkIns : Opts -> Expr -> List ( Index, Coord )
mkIns os expr =
    List.sortBy (Tuple.second) <| mkIns_multi os expr ++ mkIns_non_multi os expr


{-| Returns a list of all multiplex catch terminal coordinates.
-}
mkIns_multi : Opts -> String -> List ( Index, Coord )
mkIns_multi os expr =
    List.map (\idx -> ( idx, toCoordI os idx + os.h_off )) <| String.indices "[" expr


{-| Returns a list of all non-multiplex catch terminal coordinates.
-}
mkIns_non_multi : Opts -> Expr -> List ( Index, Coord )
mkIns_non_multi os expr =
    let
        bounds =
            List.map2 (,)
                (String.indices "[" expr)
                (String.indices "]" expr)
    in
        List.map (\i -> ( i, toCoordI os i + os.h_off )) <|
            List.filter
                (\i -> List.all (\( l, r ) -> l > i || r < i) bounds)
                (List.range 0 (os.num_blocks - 1))
