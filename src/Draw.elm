module Draw
    exposing
        ( arrowDefinition
        , arrows
        , mkViewbox
        , multiplexBoxes
        , tokenBoxes
        )

import Basics.Extra exposing (fmod)
import Either exposing (Either(Left, Right))
import List.Extra as LE
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))
import Template as T
import Template.Infix exposing ((<%), (%>))


--

import Arrow
import Lib exposing (..)


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


arrowToSvgLinear : LinearOpts -> Opts -> (Arrow -> S.Svg msg)
arrowToSvgLinear { unit, self_arrow, arrow_offset, y_delt } { canvas } =
    let
        out_coord arr =
            toFloat (arr.out_index) * unit.w

        in_coord arr =
            toFloat (arr.in_index) * unit.w

        mkLoopLinear : Arrow -> String
        mkLoopLinear arr =
            let
                x_base =
                    out_coord arr

                y_base =
                    ((canvas.w) / 2.0) - unit.h
            in
                T.render
                    (T.template "M" <% .o_x %> " " <% .o_y %> " C " <% .x_1 %> " " <% .y_1 %> ", " <% .x_2 %> " " <% .y_2 %> ", " <% .f_x %> " " <% .f_y %> "")
                    { o_x = toString x_base
                    , o_y = toString y_base
                    , x_1 = toString <| x_base - self_arrow.w
                    , y_1 = toString <| y_base - self_arrow.h
                    , x_2 = toString <| x_base + self_arrow.w
                    , y_2 = toString <| y_base - self_arrow.h
                    , f_x = toString x_base
                    , f_y = toString y_base
                    }

        mkThrowLinear : Arrow -> String
        mkThrowLinear arr =
            let
                radius =
                    abs <| (out_coord arr - in_coord arr) / 2.0

                y_base =
                    if (Arrow.bias arr) == Above then
                        canvas.w / 2.0 - unit.h
                    else
                        canvas.w / 2.0 + arrow_offset.y
            in
                T.render
                    (T.template "M" <% .origin_x %> " " <% .origin_y %> " A " <% .rx %> " " <% .ry %> " " <% .x_axis_rotation %> " " <% .large_arc_flag %> " " <% .sweep_flag %> " " <% .dx %> " " <% .dy %> "")
                    { origin_x = toString <| out_coord arr
                    , origin_y = toString y_base
                    , rx = toString radius
                    , ry = toString radius
                    , x_axis_rotation = "0"
                    , large_arc_flag = "0"
                    , sweep_flag = "1"
                    , dx =
                        toString <|
                            if arr.should_curtail_linear then
                                let
                                    dx =
                                        (out_coord arr + in_coord arr) / 2.0

                                    dy =
                                        (sqrt <| radius ^ 2 - y_delt ^ 2)
                                in
                                    ((Arrow.bias arr) == Above)
                                        ? (dx + dy)
                                    <|
                                        (dx - dy)
                            else
                                in_coord arr
                    , dy =
                        toString <|
                            if arr.should_curtail_linear then
                                if (Arrow.bias arr) == Above then
                                    y_base - y_delt
                                else
                                    y_base + y_delt
                            else
                                y_base
                    }
    in
        (\arr ->
            S.path
                [ SA.d <|
                    (Arrow.is_self arr ? mkLoopLinear <| mkThrowLinear) arr
                , SA.stroke "black"
                , SA.fill "transparent"
                , SA.strokeWidth "1.5"
                , SA.markerEnd "url(#arrow)"
                ]
                []
        )


arrowToSvgCircular : CircularOpts -> Opts -> (Arrow -> S.Svg msg)
arrowToSvgCircular { center, radius, unit, self_arrow } { num_tokens } =
    let
        mkLoopCircular : Arrow -> String
        mkLoopCircular { out_index } =
            let
                out_theta =
                    fmod
                        ((2 * pi) / (toFloat num_tokens) * (toFloat out_index))
                        (2 * pi)
            in
                T.render
                    (T.template "M" <% .o_x %> " " <% .o_y %> " C " <% .x_1 %> " " <% .y_1 %> ", " <% .x_2 %> " " <% .y_2 %> ", " <% .f_x %> " " <% .f_y %> "")
                    { o_x = toString <| center.x
                    , o_y = toString <| (center.y - radius) + (unit.h / 2)
                    , x_1 = toString <| center.x - self_arrow.w
                    , y_1 =
                        toString <|
                            (center.y - radius)
                                + (unit.h / 2)
                                + self_arrow.h
                    , x_2 = toString <| center.x + self_arrow.w
                    , y_2 =
                        toString <|
                            (center.y - radius)
                                + (unit.h / 2)
                                + self_arrow.h
                    , f_x = toString <| center.x
                    , f_y = toString <| (center.y - radius) + (unit.h / 2)
                    }

        mkThrowCircular : Arrow -> String
        mkThrowCircular { out_index, in_index } =
            let
                out_theta =
                    fmod
                        ((2 * pi) / (toFloat num_tokens) * (toFloat out_index))
                        (2 * pi)

                in_theta =
                    fmod
                        ((2 * pi) / (toFloat num_tokens) * (toFloat in_index))
                        (2 * pi)

                starting_radius =
                    radius - (unit.h / 2)

                ending_radius =
                    radius - (unit.h / 2)

                alpha_x =
                    center.x + (starting_radius * sin out_theta)

                alpha_y =
                    center.y - (starting_radius * cos out_theta)

                beta_x =
                    center.x + (ending_radius * sin in_theta)

                beta_y =
                    center.y - (ending_radius * cos in_theta)
            in
                T.render
                    (T.template "M " <% .alpha_x %> " " <% .alpha_y %> " A " <% .radius %> " " <% .radius %> " 0 0 " <% .sweep_flag %> " " <% .beta_x %> " " <% .beta_y %> "")
                    { alpha_x = toString alpha_x
                    , alpha_y = toString alpha_y
                    , beta_x = toString beta_x
                    , beta_y = toString beta_y

                    -- this is about the cross prorduct of a->b with a->center.
                    , sweep_flag =
                        if
                            ((beta_x - alpha_x)
                                * (center.y - alpha_y)
                                - (beta_y - alpha_y)
                                * (center.x - alpha_x)
                            )
                                > 0
                        then
                            "0"
                        else
                            "1"
                    , radius =
                        toString <|
                            sqrt <|
                                (beta_y - alpha_y)
                                    ^ 2
                                    + (beta_x - alpha_x)
                                    ^ 2
                    }
    in
        (\arr ->
            S.g []
                [ S.path
                    [ SA.d <|
                        (Arrow.is_self arr ? mkLoopCircular <| mkThrowCircular)
                            arr
                    , SA.stroke "black"
                    , SA.fill "transparent"
                    , SA.strokeWidth "1.5"
                    , SA.markerEnd "url(#arrow)"
                    , SA.transform <|
                        if (Arrow.is_self arr) then
                            T.render
                                (T.template "rotate(" <% .theta %> "," <% .center_x %> "," <% .center_y %> ")")
                                { theta =
                                    toString <|
                                        360.0
                                            / toFloat num_tokens
                                            * toFloat (arr.out_index)
                                , center_x = toString <| center.x
                                , center_y = toString <| center.y
                                }
                        else
                            ""
                    ]
                    []
                ]
        )



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
            d_th =
                (2 * pi)
                    / (toFloat opts.num_tokens)
                    * (toFloat <| (r_idx - l_idx))

            inner_r =
                radius - (unit.h / 2.0) + multiplex_offset.y

            outer_r =
                radius + (unit.h / 2.0) + multiplex_offset.y
        in
            S.path
                [ SA.d <|
                    T.render
                        (T.template "M " <% .alpha_x %> " " <% .alpha_y %> " L " <% .betaa_x %> " " <% .betaa_y %> " A " <% .inner_r %> " " <% .inner_r %> " 0 " <% .large_arc_inner %> " 1 " <% .gamma_x %> " " <% .gamma_y %> " L " <% .delta_x %> " " <% .delta_y %> " A " <% .outer_r %> " " <% .outer_r %> " 0 " <% .large_arc_outer %> " 0 " <% .alpha_x %> " " <% .alpha_y %> " Z")
                        { alpha_x = toString <| center.x
                        , alpha_y = toString <| center.y - outer_r
                        , betaa_x = toString <| center.x
                        , betaa_y = toString <| center.y - inner_r
                        , gamma_x = toString <| center.x + inner_r * sin d_th
                        , gamma_y = toString <| center.y - inner_r * cos d_th
                        , delta_x = toString <| center.x + outer_r * sin d_th
                        , delta_y = toString <| center.y - outer_r * cos d_th
                        , inner_r = toString <| inner_r
                        , outer_r = toString <| outer_r
                        , large_arc_inner = toString <| (d_th < pi) ? 0 <| 1
                        , large_arc_outer = toString <| (d_th < pi) ? 0 <| 1
                        }
                , SA.fill "lightblue"
                , SA.transform <|
                    T.render
                        (T.template "rotate(" <% .theta %> "," <% .center_x %> "," <% .center_y %> ")")
                        { theta =
                            toString <|
                                360.0
                                    / toFloat opts.num_tokens
                                    * toFloat (l_idx)
                        , center_x = toString <| center.x
                        , center_y = toString <| center.y
                        }
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
                T.render
                    (T.template "rotate(" <% .theta %> "," <% .center_x %> "," <% .center_y %> ")")
                    { theta =
                        toString <|
                            (360.0 / toFloat num_tokens * toFloat index)
                    , center_x = toString <| center.x
                    , center_y = toString <| center.y
                    }
            ]
            [ S.text txt ]
    )



--
