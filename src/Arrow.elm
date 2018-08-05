module Arrow
    exposing
        ( mkArrows
        , bias
        , is_self
        )

import Either exposing (Either(Left, Right))
import List.Extra as LE
import Maybe
import Maybe.Extra as ME
import Template as T
import Template.Infix exposing ((<%), (%>))
import Ternary exposing ((?))
import Unwrap as U


--

import Types exposing (..)


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
bias { out_index, in_index } =
    -- (<=) so that self-loops, which are drawn atop the character, count as
    -- conflicts
    (out_index <= in_index) ? Above <| Below


is_self : Arrow -> Bool
is_self { out_index, in_index } =
    out_index == in_index


mkArrow : Int -> Token -> Throw -> List Int -> Arrow
mkArrow out_index { catch_index } { value, is_cross, hand } catch_points =
    let
        -- this is the index in the catch list of the token's catch_index
        out_idx_in_catch =
            U.maybe <|
                LE.findIndex ((==) catch_index) catch_points

        -- this is measured in catch_point indexes,
        -- not real printed character indexes
        jump_distance =
            if is_cross then
                value
                    + case hand of
                        Center ->
                            0

                        LeftHand ->
                            1

                        RightHand ->
                            (-1)
            else
                value

        in_idx_in_catch =
            (out_idx_in_catch + jump_distance) % (List.length catch_points)
    in
        { emptyArrow
            | out_index = out_index
            , in_index =
                U.maybe <| LE.getAt in_idx_in_catch catch_points
        }


mkArrows : List Token -> List Arrow
mkArrows tokens =
    let
        catch_points : List Int
        catch_points =
            LE.unique <|
                List.map .catch_index <|
                    List.filter .is_catch tokens
    in
        addColors <|
            addCurtailsCircular <|
                addCurtailsLinear <|
                    List.concat <|
                        List.map
                            (\( idx, token ) ->
                                case token.throw of
                                    Just throw ->
                                        if throw.is_sink then
                                            []
                                        else
                                            [ mkArrow idx token throw catch_points ]

                                    Nothing ->
                                        []
                            )
                        <|
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
                List.filter (\x -> arr.in_index == x.out_index || arr.in_index == x.in_index) <|
                    LE.remove arr arrs
    in
        List.map (\x -> { x | should_curtail_circular = not (hasConflict x) }) arrs



--
