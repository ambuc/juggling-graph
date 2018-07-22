module Arrow
    exposing
        ( mkArrows
        , bias
        , is_self
        )

import List.Extra as LE
import Maybe
import Either exposing (Either(Left, Right))
import Maybe.Extra as ME
import Ternary exposing ((?))
import Template as T
import Template.Infix exposing ((<%), (%>))


--

import Lib exposing (..)


--------------------------------------------------------------------------------
-- TYPE ZONE -------------------------------------------------------------------
--------------------------------------------------------------------------------


emptyArrow : Arrow
emptyArrow =
    { out_index = 0
    , in_index = 0
    , should_curtail_linear = False
    , should_curtail_circular = False
    }



--------------------------------------------------------------------------------
-- GENERATION ZONE -------------------------------------------------------------
--------------------------------------------------------------------------------


bias : Arrow -> Bias
bias arr =
    -- (<=) so that self-loops, which are drawn atop the character, count as
    -- conflicts
    (arr.out_index <= arr.in_index) ? Above <| Below


is_self : Arrow -> Bool
is_self arr =
    arr.out_index == arr.in_index


mkArrows : List Token -> List Arrow
mkArrows tokens =
    let
        max_recv_index =
            Maybe.withDefault 0 <| List.maximum <| List.map .recv_index tokens

        recv_idx_list : List Int
        recv_idx_list =
            LE.unique <| List.map .recv_index <| List.filter .is_recv tokens
    in
        addColors <|
            addCurtailsLinear <|
                addCurtailsCircular <|
                    List.map
                        (\( out_index, token ) ->
                            let
                                -- this is the index in the recv list of the token's recv_index
                                out_idx_in_recv =
                                    Maybe.withDefault 0 <|
                                        LE.findIndex (\x -> x == token.recv_index) recv_idx_list

                                -- this is measured in recv_point indexes,
                                -- not real printed character indexes
                                jump_distance =
                                    let
                                        default_distance =
                                            ME.unwrap 0 .value token.throw

                                        addtl_offset =
                                            if (ME.unwrap False .is_cross token.throw) then
                                                case (ME.unwrap Center .hand token.throw) of
                                                    Center ->
                                                        0

                                                    LeftHand ->
                                                        1

                                                    RightHand ->
                                                        (-1)
                                            else
                                                0
                                    in
                                        default_distance + addtl_offset

                                in_idx_in_recv =
                                    (out_idx_in_recv + jump_distance)
                                        % (List.length recv_idx_list)

                                in_idx =
                                    Maybe.withDefault 0 <|
                                        LE.getAt in_idx_in_recv recv_idx_list
                            in
                                { emptyArrow
                                    | out_index = out_index
                                    , in_index = in_idx
                                }
                        )
                    <|
                        List.filter (\( _, token ) -> ME.isJust token.throw) <|
                            List.indexedMap (,) tokens


{-| TODO(jbuckland) Implement colors
-}
addColors : List Arrow -> List Arrow
addColors ls =
    ls


addCurtailsLinear : List Arrow -> List Arrow
addCurtailsLinear arrs =
    let
        hasConflict : Arrow -> Bool
        hasConflict arr =
            List.isEmpty <|
                List.filter (\x -> arr.in_index == x.out_index) <|
                    List.filter (\x -> bias x == bias arr) <|
                        LE.remove arr arrs
    in
        List.map (\x -> { x | should_curtail_linear = not (hasConflict x) }) arrs


addCurtailsCircular : List Arrow -> List Arrow
addCurtailsCircular arrs =
    let
        hasConflict : Arrow -> Bool
        hasConflict arr =
            List.isEmpty <|
                List.filter (\x -> arr.in_index == x.out_index) <|
                    LE.remove arr arrs
    in
        List.map (\x -> { x | should_curtail_circular = not (hasConflict x) }) arrs



--
