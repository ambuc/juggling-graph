module Tokens exposing (mkBeatmap)

import List.Extra as LE
import Ternary exposing ((?))


--

import Lib exposing (..)


emptyToken : Token
emptyToken =
    { txt = ""
    , throw = Nothing
    , recv_index = 0
    , is_recv = False
    }


toToken : Char -> Token
toToken x =
    { emptyToken | txt = String.fromList [ x ] }


markRecv : Token -> Token
markRecv t =
    { t | is_recv = True }


mkTokensBeat : Int -> Beat -> List Token
mkTokensBeat recv_index beat =
    let
        base =
            List.map
                (\throw ->
                    { emptyToken
                        | txt =
                            String.fromList <|
                                throw.char
                                    :: (throw.is_cross ? [ 'x' ] <| [])
                        , throw = Just throw
                        , recv_index = recv_index
                    }
                )
                beat.throws
    in
        if (List.length beat.throws == 1) then
            List.map (\b -> { b | is_recv = True }) base
        else
            [ { emptyToken
                | txt = String.fromList [ '[' ]
                , is_recv = True
                , recv_index = recv_index
              }
            ]
                ++ base
                ++ [ toToken ']' ]


mkBeatmapFold : Int -> BeatMap -> List Token
mkBeatmapFold recv_index beatmap =
    case (LE.uncons beatmap) of
        Just ( [ hand ], tail ) ->
            let
                hand_length =
                    if (List.length hand.throws == 1) then
                        1
                    else
                        2 + List.length hand.throws
            in
                mkTokensBeat recv_index hand
                    ++ mkBeatmapFold (recv_index + hand_length) tail

        Just ( [ left, right ], tail ) ->
            let
                left_length =
                    if (List.length left.throws == 1) then
                        1
                    else
                        2 + List.length left.throws

                right_length =
                    if (List.length right.throws == 1) then
                        1
                    else
                        2 + List.length right.throws
            in
                [ toToken '(' ]
                    ++ mkTokensBeat (recv_index + 1) left
                    ++ [ toToken ',' ]
                    ++ mkTokensBeat (recv_index + left_length + 2) right
                    ++ [ toToken ')' ]
                    ++ mkBeatmapFold
                        (recv_index
                            + left_length
                            + right_length
                            + 3
                        )
                        tail

        _ ->
            []


mkBeatmap : BeatMap -> List Token
mkBeatmap beatmap =
    mkBeatmapFold 0 beatmap
