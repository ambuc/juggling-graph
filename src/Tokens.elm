module Tokens exposing (mkTokens)

import List.Extra as LE
import Ternary exposing ((?))


--

import Lib exposing (..)


------------
-- TOKENS --
------------
-- Tokens are a flattened representation of a beatmap. We unfold a beatmap into
-- its component beats and throws, annotating each throw with metadata useful
-- for both computing throws and printing. A token is the smallest unit width
-- for printing. Tokens generally map to characters in an input string, except
-- that the `x` character in sync gets attached to its left-adjacent character.


emptyToken : Token
emptyToken =
    { txt = ""
    , throw = Nothing
    , recv_index = 0
    , is_recv = False
    }


syntaxToToken : Char -> Token
syntaxToToken x =
    { emptyToken | txt = String.fromList [ x ] }


throwToToken : Int -> Throw -> Token
throwToToken curr_idx throw =
    { emptyToken
        | txt = String.fromList <| throw.char :: (throw.is_cross ? [ 'x' ] <| [])
        , throw = Just throw
        , recv_index = curr_idx
    }


beatToToken : Int -> Beat -> List Token
beatToToken curr_idx { throws } =
    let
        left_bracket =
            [ { emptyToken
                | txt = String.fromList [ '[' ]
                , is_recv = True
                , recv_index = curr_idx
              }
            ]

        inner =
            List.map (throwToToken curr_idx) throws

        right_bracket =
            [ syntaxToToken ']' ]
    in
        if (List.length throws == 1) then
            List.map (\b -> { b | is_recv = True }) inner
        else
            List.concat [ left_bracket, inner, right_bracket ]


chr_len : Beat -> Int
chr_len { throws } =
    (List.length throws == 1) ? 1 <| (2 + List.length throws)


beatmapToToken : Int -> List (List Beat) -> List Token
beatmapToToken curr_idx beatmap =
    case (LE.uncons beatmap) of
        Just ( [ hand ], tail ) ->
            List.concat
                [ beatToToken curr_idx hand
                , beatmapToToken (curr_idx + chr_len hand) tail
                ]

        Just ( [ left, right ], tail ) ->
            List.concat
                [ [ syntaxToToken '(' ]
                , beatToToken (curr_idx + 1) left
                , [ syntaxToToken ',' ]
                , beatToToken (curr_idx + chr_len left + 2) right
                , [ syntaxToToken ')' ]
                , beatmapToToken (curr_idx + chr_len left + chr_len right + 3) tail
                ]

        _ ->
            []


mkTokens : List (List Beat) -> List Token
mkTokens beatmap =
    beatmapToToken 0 beatmap
