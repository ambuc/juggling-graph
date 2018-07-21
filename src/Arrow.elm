module Arrow
    exposing
        ( definition
        , toSvgArrow
        , addCurtails
        , addColors
        , mkDescriptors
        )

import List.Extra as LE
import Maybe
import Maybe.Extra as ME
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))
import Template as T
import Template.Infix exposing ((<%), (%>))


--

import Lib exposing (..)


--------------------------------------------------------------------------------
-- TYPE ZONE -------------------------------------------------------------------
--------------------------------------------------------------------------------


type alias Descriptor =
    { out_index : Int
    , out_coord : Float
    , in_index : Int
    , in_coord : Float
    , should_curtail : Bool
    }


emptyDescriptor : Descriptor
emptyDescriptor =
    { out_index = 0
    , out_coord = 0.0
    , in_index = 0
    , in_coord = 0.0
    , should_curtail = False
    }


type Bias
    = Above
    | Below



--------------------------------------------------------------------------------
-- GENERATION ZONE -------------------------------------------------------------
--------------------------------------------------------------------------------


bias : Descriptor -> Bias
bias arr =
    -- (<=) so that self-loops, which are drawn atop the character, count as
    -- conflicts
    (arr.out_coord <= arr.in_coord) ? Above <| Below


is_self : Descriptor -> Bool
is_self arr =
    arr.out_coord == arr.in_coord


mkDescriptors : Opts -> List Token -> List Descriptor
mkDescriptors opts tokens =
    let
        max_recv_index =
            Maybe.withDefault 0 <| List.maximum <| List.map .recv_index tokens

        recv_idx_list : List Int
        recv_idx_list =
            LE.unique <| List.map .recv_index <| List.filter .is_recv tokens
    in
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

                                        Left ->
                                            1

                                        Right ->
                                            (-1)
                                else
                                    0
                        in
                            default_distance + addtl_offset

                    --if opts.is_sync then value // 2 else value
                    in_idx_in_recv =
                        (out_idx_in_recv + jump_distance)
                            % (List.length recv_idx_list)

                    in_idx =
                        Maybe.withDefault 0 <|
                            LE.getAt in_idx_in_recv recv_idx_list
                in
                    { emptyDescriptor
                        | out_index = out_index
                        , out_coord = toFloat out_index * opts.unit.w
                        , in_index = in_idx
                        , in_coord = toFloat (in_idx) * opts.unit.w
                    }
            )
        <|
            List.filter (\( _, token ) -> ME.isJust token.throw) <|
                List.indexedMap (,) tokens


{-| TODO(jbuckland) Implement colors
-}
addColors : List Descriptor -> List Descriptor
addColors ls =
    ls


addCurtails : List Descriptor -> List Descriptor
addCurtails arrs =
    let
        hasConflict : Descriptor -> Bool
        hasConflict arr =
            List.isEmpty <|
                List.filter (\x -> arr.in_index == x.out_index) <|
                    List.filter (\x -> bias x == bias arr) <|
                        LE.remove arr arrs
    in
        List.map (\x -> { x | should_curtail = not (hasConflict x) }) arrs



--------------------------------------------------------------------------------
-- DRAWING ZONE ----------------------------------------------------------------
--------------------------------------------------------------------------------


{-| The svg definition for a standard arrowhead.
-}
definition : S.Svg msg
definition =
    S.marker
        [ SA.id "arrow"
        , SA.markerWidth "10"
        , SA.markerHeight "10"
        , SA.refX "7"
        , SA.refY "3"
        , SA.orient "auto"
        , SA.markerUnits "strokeWidth"
        ]
        [ S.path [ SA.d "M0,0 L0,6 L9,3 z", SA.fill "black" ] []
        ]


{-| Returns a standard semicircular throwing arrow.
-}
toSvgArrow : Opts -> Descriptor -> S.Svg msg
toSvgArrow opts arr =
    S.path
        [ SA.d <| (is_self arr ? mkPathSelf <| mkPathThrown) opts arr
        , SA.stroke "black"
        , SA.fill "transparent"
        , SA.strokeWidth "1.0"
        , SA.markerEnd "url(#arrow)"
        ]
        []


{-| Returns a specialized identity throwing arrow.
-}
mkPathSelf : Opts -> Descriptor -> String
mkPathSelf { canvas, unit, self_arrow } arr =
    let
        x_base =
            arr.out_coord

        y_base =
            ((canvas.w) / 2.0) - unit.h
    in
        T.render
            (T.template "M" <% .p1x %> " " <% .p1y %> " C " <% .p2x %> " " <% .p2y %> ", " <% .p3x %> " " <% .p3y %> ", " <% .p4x %> " " <% .p4y %> "")
            { p1x = toString x_base
            , p1y = toString y_base
            , p2x = toString <| x_base - self_arrow.w
            , p2y = toString <| y_base - self_arrow.h
            , p3x = toString <| x_base + self_arrow.w
            , p3y = toString <| y_base - self_arrow.h
            , p4x = toString x_base
            , p4y = toString y_base
            }


mkPathThrown : Opts -> Descriptor -> String
mkPathThrown { canvas, unit, arrow_offset, y_delt } arr =
    let
        radius =
            abs <| (arr.out_coord - arr.in_coord) / 2.0

        y_base =
            let
                default_y_base =
                    canvas.w / 2.0
            in
                if (bias arr) == Above then
                    default_y_base - unit.h
                else
                    default_y_base + arrow_offset.y
    in
        T.render
            (T.template "M" <% .origin_x %> " " <% .origin_y %> " A " <% .rx %> " " <% .ry %> " " <% .x_axis_rotation %> " " <% .large_arc_flag %> " " <% .sweep_flag %> " " <% .dx %> " " <% .dy %> "")
            { origin_x = toString arr.out_coord
            , origin_y = toString y_base
            , rx = toString radius
            , ry = toString radius
            , x_axis_rotation = "0"
            , large_arc_flag = "0"
            , sweep_flag = "1"
            , dx =
                toString <|
                    if arr.should_curtail then
                        let
                            dx =
                                (arr.out_coord + arr.in_coord) / 2.0

                            dy =
                                (sqrt <| radius ^ 2 - y_delt ^ 2)
                        in
                            ((bias arr) == Above) ? (dx + dy) <| (dx - dy)
                    else
                        arr.in_coord
            , dy =
                toString <|
                    if arr.should_curtail then
                        ((bias arr) == Above) ? (y_base - y_delt) <| (y_base + y_delt)
                    else
                        y_base
            }
