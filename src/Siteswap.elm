module Siteswap exposing (renderExpr)

{-| Experimental package for visualizing siteswaps.

This library accepts a plaintext expression and returns a rendered siteswap
notation image of it. In the context of juggling notation, siteswap is a
notation used to encode juggling patterns.


# renderExpr

@docs renderExpr

-}

import Debug as DBG
import Html exposing (..)
import List.Extra as LE
import Debug as DBG
import Svg as S
import Svg.Attributes as SA


--

import Lib exposing (..)
import XVals as XVals


{-| Places a unit block, given a character and coordinate.
-}
place_block : Opts -> String -> Float -> S.Svg msg
place_block os chr coord =
    S.text_
        [ SA.x <| toString <| coord - os.h_off
        , SA.y <| toString os.cv_w_2
        , SA.fontSize <| toString os.unit_h
        , SA.fill "black"
        ]
        [ S.text chr ]


{-| Places all unit blocks, given the input expression.
-}
place_blocks : Opts -> Expr -> S.Svg msg
place_blocks os expr =
    S.g [] <|
        List.map2
            (\chr coord ->
                S.g []
                    [ render_rect
                        ( coord
                        , os.cv_w_2 - (os.unit_h / 2.0) + (os.v_off / 2.0)
                        )
                        ( os.unit_w - 1.0, os.unit_h )
                        15
                        "#eee"
                        0.4
                    , place_block os chr coord
                    ]
            )
            (String.split "" expr)
            (List.map (\x -> toCoordI os x)
                (List.range 0 (os.num_blocks - 1))
            )


{-| Places the light blue multiplex boundaries on the canvas.
-}
place_multiplex_boundaries : Opts -> Expr -> S.Svg msg
place_multiplex_boundaries os expr =
    S.g [] <|
        List.map2
            (\l r ->
                render_rect
                    ( ((toCoordF os r) + (toCoordF os l)) / 2.0
                    , os.cv_w_2 - (os.unit_h / 2.0) + (os.v_off / 2.0)
                    )
                    ( ((r - l) * os.unit_w) - 1.0, 2 * os.unit_h )
                    5
                    "lightblue"
                    1.0
            )
            (List.map toFloat <| String.indices "[" expr)
            (List.map toFloat <| String.indices "]" expr)



------------------------------------------------------------------


{-| Plans a single throw arrows. Returns a single descriptor.
-}
plan_arrow : Opts -> List ( Index, Coord ) -> Float -> Int -> ArrowDescriptor
plan_arrow os xins out_coord jump_dist =
    let
        -- multiplex-level index of the out position in xins
        out_idx : Int
        out_idx =
            (List.length <|
                Tuple.first <|
                    LE.break ((<) out_coord) <|
                        List.map
                            Tuple.second
                            xins
            )
                - 1

        -- multiplex-level index of the in position in xins
        in_idx : Int
        in_idx =
            (jump_dist + out_idx) % (List.length xins)

        -- cartesian coordinate of the in position
        in_coord : Float
        in_coord =
            case (LE.getAt in_idx xins) of
                Just ( _, x ) ->
                    x

                Nothing ->
                    DBG.crash "unreachable"
    in
        { out_index = out_idx
        , out_coord = out_coord
        , in_index = in_idx
        , in_coord = in_coord
        }


{-| Plans all throw arrows. Returns a list of descriptors.
-}
plan_arrows : Opts -> Expr -> List ArrowDescriptor
plan_arrows os expr =
    let
        xins =
            XVals.mkIns os expr
    in
        List.map (\( _, coord, value ) -> plan_arrow os xins coord value) <|
            XVals.mkOuts os expr


{-| Places one throw arrow, with context of all throw arrows, to avoid conflict.
-}
place_arrow : Opts -> List ArrowDescriptor -> ArrowDescriptor -> S.Svg msg
place_arrow os all_arrows arr =
    let
        is_conflict =
            not <|
                List.isEmpty <|
                    List.filter (\x -> arr.in_coord == x.out_coord) <|
                        List.filter
                            (\x ->
                                bias x.out_coord x.in_coord
                                    == bias arr.out_coord arr.in_coord
                            )
                        <|
                            LE.remove arr all_arrows
    in
        if arr.out_coord == arr.in_coord then
            self_arrow os arr.in_coord
        else if arr.in_coord < arr.out_coord then
            throw_arrow os arr (os.cv_w_2 + os.v_off) is_conflict
        else
            throw_arrow os arr (os.cv_w_2 - os.unit_h) is_conflict


{-| Places all throw arrows.
-}
place_arrows : Opts -> Expr -> S.Svg msg
place_arrows os expr =
    let
        all_arrows =
            DBG.log "all arrows:" <| plan_arrows os expr
    in
        S.g [] <| List.map (place_arrow os all_arrows) <| all_arrows


{-| Generates an svg of the given siteswap.

    renderExpr <canvas width> <expression>
    renderExpr 500 "3[12][22]"

-}
renderExpr : Int -> Expr -> Html.Html msg
renderExpr canvas_width expr =
    let
        num_blocks_ =
            (String.length expr)

        os : Opts
        os =
            { num_blocks = num_blocks_
            , cv_w = canvas_width
            , cv_w_2 = toFloat canvas_width / 2.0
            , unit_w = (toFloat canvas_width) / (toFloat num_blocks_)
            , unit_w_2 = ((toFloat canvas_width) / (toFloat num_blocks_)) / 2.0
            , unit_h = 30.0
            , v_off = 14.0
            , h_off = 4.0
            , self_arrow_w = 25
            , self_arrow_h = 35
            , y_delt = 15
            }
    in
        S.svg
            [ SA.width <| toString os.cv_w
            , SA.height <| toString os.cv_w
            , SA.viewBox
                ("0 0 "
                    ++ toString os.cv_w
                    ++ " "
                    ++ toString os.cv_w
                )
            ]
            [ S.defs [] [ arrow_def ]
            , place_multiplex_boundaries os expr
            , place_blocks os expr
            , place_arrows os expr
            ]
