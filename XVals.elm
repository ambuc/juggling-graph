module XVals exposing (mkXIns, mkXOuts)

import Result


--

import Lib exposing (..)


-- XOUTS


type alias XOuts =
    List ( Float, Int )


{-| Returns a sorted list of all throw terminal coordinates.
-}
mkXOuts : Opts -> Expr -> XOuts
mkXOuts os expr =
    List.map
        (\( idx, chr ) ->
            ( toCoordI os idx + os.h_off
            , Result.withDefault 0 <| String.toInt chr
            )
        )
    <|
        List.filter (\( _, chr ) -> chr /= "]" && chr /= "[") <|
            List.indexedMap (,) (String.split "" expr)



-- XINS


type alias XIns =
    List Float


{-| Returns a sorted list of all catch terminal coordinates.
-}
mkXIns : Opts -> Expr -> XIns
mkXIns os expr =
    List.sort <| mkXIns_multi os expr ++ mkXIns_non_multi os expr


{-| Returns a list of all multiplex catch terminal coordinates.
-}
mkXIns_multi : Opts -> String -> XIns
mkXIns_multi os expr =
    List.map (\idx -> toCoordI os idx + os.h_off) <| String.indices "[" expr


{-| Returns a list of all non-multiplex catch terminal coordinates.
-}
mkXIns_non_multi : Opts -> Expr -> XIns
mkXIns_non_multi os expr =
    let
        bounds =
            List.map2 (,)
                (String.indices "[" expr)
                (String.indices "]" expr)
    in
        List.map (\i -> toCoordI os i + os.h_off) <|
            List.filter
                (\i -> List.all (\( l, r ) -> l > i || r < i) bounds)
                (List.range 0 (os.num_blocks - 1))
