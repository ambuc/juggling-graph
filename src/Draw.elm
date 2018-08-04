module Draw
    exposing
        ( arrowDefinition
        , arrows
        , mkViewbox
        , multiplexBoxes
        , tokenBoxes
        )

import Either exposing (Either(Left, Right))
import List.Extra as LE
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))
import Template as T
import Template.Infix exposing ((<%), (%>))


--

import Arrow
import Points exposing (..)
import Types exposing (..)


--------------------
-- MATH FUNCTIONS --
--------------------


tau : Float
tau =
    2 * pi


slope : XY -> XY -> Float
slope a b =
    atan2 (b.y - a.y) (b.x - a.x)


midpoint : XY -> XY -> XY
midpoint a b =
    { x = (a.x + b.x) / 2.0, y = (a.y + b.y) / 2.0 }


{-| Returns the sign of the cross product of o->a with o->b.
-}
sign_of_cross : XY -> XY -> XY -> Float
sign_of_cross o a b =
    (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)


{-| Distance fn.
-}
dist : XY -> XY -> Float
dist a b =
    sqrt <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2


{-| Returns one of a or b, whichever is closest to o.
-}
closest_to : XY -> XY -> XY -> XY
closest_to o a b =
    (dist b o > dist a o) ? a <| b


{-| Returns one of a or b, whichever is furthest from o.
-}
furthest_from : XY -> XY -> XY -> XY
furthest_from o a b =
    (dist b o > dist a o) ? b <| a



-----------------------
-- DRAWING FUNCTIONS --
-----------------------


curveString : XY -> XY -> XY -> XY -> String
curveString pt_0 pt_1 pt_2 pt_3 =
    T.render
        (T.template "M" <% .pt_0_x %> " " <% .pt_0_y %> " C " <% .pt_1_x %> " " <% .pt_1_y %> ", " <% .pt_2_x %> " " <% .pt_2_y %> ", " <% .pt_3_x %> " " <% .pt_3_y %> "")
        { pt_0_x = toString pt_0.x
        , pt_0_y = toString pt_0.y
        , pt_1_x = toString pt_1.x
        , pt_1_y = toString pt_1.y
        , pt_2_x = toString pt_2.x
        , pt_2_y = toString pt_2.y
        , pt_3_x = toString pt_3.x
        , pt_3_y = toString pt_3.y
        }


arcString : XY -> Float -> Bool -> Bool -> Bool -> XY -> String
arcString pt_i radius x_axis_rotation large_arc sweep pt_f =
    T.render
        (T.template "M " <% .pt_i_x %> " " <% .pt_i_y %> " A " <% .radius %> " " <% .radius %> " " <% .xaxisf %> " " <% .largef %> " " <% .sweepf %> " " <% .pt_f_x %> " " <% .pt_f_y %> "")
        { pt_i_x = toString pt_i.x
        , pt_i_y = toString pt_i.y
        , radius = toString radius
        , sweepf = toString <| sweep ? 1 <| 0
        , largef = toString <| large_arc ? 1 <| 0
        , xaxisf = toString <| x_axis_rotation ? 1 <| 0
        , pt_f_x = toString pt_f.x
        , pt_f_y = toString pt_f.y
        }


rotateAbout : Float -> XY -> String
rotateAbout t c =
    T.render
        (T.template "rotate(" <% .t %> "," <% .c_x %> "," <% .c_y %> ")")
        { t = toString <| t
        , c_x = toString <| c.x
        , c_y = toString <| c.y
        }


curvedRectangle : XY -> XY -> XY -> XY -> Float -> Float -> Float -> String
curvedRectangle o_a i_a i_b o_b o_r i_r angle =
    T.render
        (T.template "M " <% .o_a_x %> " " <% .o_a_y %> " L " <% .i_a_x %> " " <% .i_a_y %> " A " <% .i_r %> " " <% .i_r %> " 0 " <% .large_arc_i %> " 1 " <% .i_b_x %> " " <% .i_b_y %> " L " <% .o_b_x %> " " <% .o_b_y %> " A " <% .o_r %> " " <% .o_r %> " 0 " <% .large_arc_o %> " 0 " <% .o_a_x %> " " <% .o_a_y %> " Z")
        { o_a_x = toString <| o_a.x
        , o_a_y = toString <| o_a.y
        , i_a_x = toString <| i_a.x
        , i_a_y = toString <| i_a.y
        , i_b_x = toString <| i_b.x
        , i_b_y = toString <| i_b.y
        , o_b_x = toString <| o_b.x
        , o_b_y = toString <| o_b.y
        , i_r = toString <| i_r
        , o_r = toString <| o_r
        , large_arc_i = toString <| (angle < pi) ? 0 <| 1
        , large_arc_o = toString <| (angle < pi) ? 0 <| 1
        }



-----------------------
-- VIEWBOX FUNCTIONS --
-----------------------


mkViewbox : Opts -> S.Attribute msg
mkViewbox { viewbox_offset, canvas } =
    SA.viewBox <|
        T.render
            (T.template "" <% .min_x %> " " <% .min_y %> " " <% .width %> " " <% .height %> "")
            { min_x = toString <| viewbox_offset.x
            , min_y = toString <| viewbox_offset.y
            , width = toString <| canvas.w
            , height = toString <| canvas.h
            }



-----------------------------
-- ARROW-DRAWING FUNCTIONS --
-----------------------------


{-| The svg definition for a standard arrowhead.
-}
arrowDefinition : S.Svg msg
arrowDefinition =
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


{-| Takes in a parseObject and adds arrow colors, computes conflicts and adds
curtails, and finally converts each arrow into an SVG.
-}
arrows : Opts -> ParseObject -> S.Svg msg
arrows opts parseObject =
    if not parseObject.is_valid then
        S.g [] []
    else
        S.g [] <|
            List.map
                (case opts.view_opts of
                    Left linear_opts ->
                        arrowToSvgLinear linear_opts opts

                    Right circular_opts ->
                        arrowToSvgCircular circular_opts opts
                )
                (Arrow.mkArrows parseObject.tokens)


mkLoopLinear : Opts -> LinearOpts -> Arrow -> String
mkLoopLinear { canvas } { unit, self_arrow } arr =
    let
        out_coord arr =
            toFloat (arr.out_index) * unit.w

        base : XY
        base =
            { x = out_coord arr
            , y = ((canvas.w) / 2.0) - unit.h
            }
    in
        curveString base
            (base -.- self_arrow)
            (base +.+ flipY self_arrow)
            base


mkThrowLinear : Opts -> LinearOpts -> Arrow -> String
mkThrowLinear { canvas } { unit, arrow_offset, y_delt } arr =
    let
        out_coord arr =
            toFloat (arr.out_index) * unit.w

        in_coord arr =
            toFloat (arr.in_index) * unit.w

        radius =
            abs <| (out_coord arr - in_coord arr) / 2.0

        base : XY
        base =
            { x = 0
            , y =
                if (Arrow.bias arr) == Above then
                    canvas.w / 2.0 - unit.h
                else
                    canvas.w / 2.0 + arrow_offset.y
            }

        final : XY
        final =
            { x = in_coord arr, y = base.y }

        curtailed_final : XY
        curtailed_final =
            let
                dx =
                    (out_coord arr + in_coord arr) / 2.0

                dy =
                    (sqrt <| radius ^ 2 - y_delt ^ 2)
            in
                if ((Arrow.bias arr) == Above) then
                    { x = dx + dy
                    , y = base.y - y_delt
                    }
                else
                    { x = dx - dy
                    , y = base.y + y_delt
                    }

        pt_i : XY
        pt_i =
            { x = out_coord arr, y = base.y }

        pt_f : XY
        pt_f =
            arr.should_curtail_linear ? curtailed_final <| final
    in
        arcString pt_i radius False False True pt_f


arrowToSvgLinear : LinearOpts -> Opts -> Arrow -> S.Svg msg
arrowToSvgLinear linear_opts opts arr =
    S.path
        [ SA.d <|
            (Arrow.is_self arr ? mkLoopLinear <| mkThrowLinear)
                opts
                linear_opts
                arr
        , SA.stroke "black"
        , SA.fill "transparent"
        , SA.strokeWidth "1.0"
        , SA.markerEnd "url(#arrow)"
        ]
        []


mkLoopCircular : Opts -> CircularOpts -> Arrow -> String
mkLoopCircular { canvas } { radius, center, self_arrow, unit } { out_index } =
    let
        pt_0 : XY
        pt_0 =
            { x = center.x
            , y = (center.y - radius) + (unit.h / 2)
            }
    in
        curveString
            pt_0
            (pt_0 +.+ flipX self_arrow)
            (pt_0 +.+ self_arrow)
            pt_0


mkThrowCircular : Opts -> CircularOpts -> Arrow -> String
mkThrowCircular { canvas, num_tokens } { center, unit, radius } arr =
    let
        a : XY
        a =
            center
                +/+ ( (tau / (toFloat num_tokens) * (toFloat arr.out_index))
                    , radius - (unit.h / 2)
                    )

        b : XY
        b =
            center
                +/+ ( (tau / (toFloat num_tokens) * (toFloat arr.in_index))
                    , radius - (unit.h / 2)
                    )

        r_c =
            (dist a b) / 1.25

        rp_c =
            sqrt <|
                (r_c ^ 2 - ((dist a b) / 2.0) ^ 2)

        c : XY
        c =
            furthest_from
                center
                (midpoint a b +/+ ( slope a b, rp_c ))
                (midpoint a b +/+ ( slope a b, -rp_c ))

        r_b =
            15.0

        bp : XY
        bp =
            let
                theta =
                    acos <| (2 * r_c ^ 2 - r_b ^ 2) / (2 * r_c ^ 2)
            in
                closest_to center
                    (c +/+ ( pi / 2 + slope c b + theta, r_c ))
                    (c +/+ ( pi / 2 + slope c b - theta, r_c ))

        sweep_in =
            if arr.should_curtail_circular then
                sign_of_cross a b c > 0
            else
                sign_of_cross a b center < 0

        e : XY
        e =
            arr.should_curtail_circular ? bp <| b
    in
        arcString a r_c False False sweep_in e


arrowToSvgCircular : CircularOpts -> Opts -> Arrow -> S.Svg msg
arrowToSvgCircular circular_opts opts arr =
    S.g []
        [ S.path
            [ SA.d <|
                (Arrow.is_self arr ? mkLoopCircular <| mkThrowCircular)
                    opts
                    circular_opts
                    arr
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.strokeWidth "1.0"
            , SA.markerEnd "url(#arrow)"
            , SA.transform <|
                if (Arrow.is_self arr) then
                    rotateAbout
                        (360.0 / toFloat opts.num_tokens * toFloat arr.out_index)
                        circular_opts.center
                else
                    ""
            ]
            []
        ]



------------------------------------------
-- MULTIPLEX-BOUNDARY-DRAWING FUNCTIONS --
------------------------------------------


multiplexBoxes : Opts -> ParseObject -> S.Svg msg
multiplexBoxes opts { tokens } =
    S.g [] <|
        List.map2
            (case opts.view_opts of
                Left linear_opts ->
                    boundToMultiplexBoxLinear linear_opts opts

                Right circular_opts ->
                    boundsToMultiplexBoxCircular circular_opts opts
            )
            (LE.findIndices (\x -> x.txt == "[") tokens)
            (LE.findIndices (\x -> x.txt == "]") tokens)


boundToMultiplexBoxLinear : LinearOpts -> Opts -> (Int -> Int -> S.Svg msg)
boundToMultiplexBoxLinear { unit } { canvas } =
    (\l_idx r_idx ->
        S.rect
            [ SA.x <| toString <| (toFloat l_idx * unit.w)
            , SA.y <| toString <| (canvas.w / 2.0) - (unit.h / 2.0)
            , SA.width <| toString (toFloat (r_idx - l_idx) * unit.w)
            , SA.height <| toString <| unit.h
            , SA.fill "lightblue"
            ]
            []
    )


boundsToMultiplexBoxCircular : CircularOpts -> Opts -> (Int -> Int -> S.Svg msg)
boundsToMultiplexBoxCircular { center, radius, unit, multiplex_offset } opts =
    (\l_idx r_idx ->
        let
            r_th =
                tau / toFloat opts.num_tokens * toFloat r_idx

            l_th =
                tau / toFloat opts.num_tokens * toFloat l_idx

            inner_r =
                radius - (unit.h / 2.0) + multiplex_offset.y

            outer_r =
                radius + (unit.h / 2.0) + multiplex_offset.y
        in
            S.path
                [ SA.d <|
                    curvedRectangle
                        (center +/+ ( l_th, outer_r ))
                        (center +/+ ( l_th, inner_r ))
                        (center +/+ ( r_th, inner_r ))
                        (center +/+ ( r_th, outer_r ))
                        outer_r
                        inner_r
                        (r_th - l_th)
                , SA.fill "lightblue"
                ]
                []
    )



--------------------------------------
-- TOKEN-BOUNDARY-DRAWING FUNCTIONS --
--------------------------------------


tokenBoxes : Opts -> ParseObject -> S.Svg msg
tokenBoxes opts { tokens } =
    S.g [] <|
        List.map
            (case opts.view_opts of
                Left linear_opts ->
                    indexedTokenToSvgLinear linear_opts opts

                Right circular_opts ->
                    indexedTokenToSvgCircular circular_opts opts
            )
            (List.indexedMap (,) tokens)


indexedTokenToSvgLinear : LinearOpts -> Opts -> (( Int, Token ) -> S.Svg msg)
indexedTokenToSvgLinear { text_offset, unit } { canvas } =
    (\( index, { is_valid, txt } ) ->
        S.text_
            [ SA.x <| toString <| text_offset.x + unit.w * toFloat index
            , SA.y <| toString <| text_offset.y + (canvas.w / 2.0)
            , SA.fontSize <| toString unit.h

            -- is ME.isJust (token.throw) && throw.is_valid
            -- defaults to True, so black is the default color
            , SA.fill <| is_valid ? "black" <| "red"
            , SA.textAnchor "middle"
            ]
            [ S.text txt ]
    )


indexedTokenToSvgCircular :
    CircularOpts
    -> Opts
    -> (( Int, Token ) -> S.Svg msg)
indexedTokenToSvgCircular { radius, unit, center } { canvas, num_tokens } =
    (\( index, { is_valid, txt } ) ->
        S.text_
            [ SA.x <| toString center.x
            , SA.y <| toString <| center.y - radius
            , SA.fontSize <| toString unit.h
            , SA.fill <| is_valid ? "black" <| "red"
            , SA.textAnchor "middle"
            , SA.transform <|
                rotateAbout ((360.0 / toFloat num_tokens * toFloat index)) center
            ]
            [ S.text txt ]
    )



--
