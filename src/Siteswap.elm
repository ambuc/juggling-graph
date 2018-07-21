module Siteswap exposing (renderExpr)

{-| Experimental package for visualizing siteswaps.

This library accepts a plaintext expression and returns a rendered siteswap
notation image of it. In the context of juggling notation, siteswap is a
notation used to encode juggling patterns.


# renderExpr

@docs renderExpr

-}

import Html exposing (..)
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))


--

import Arrow
import Draw
import Lib exposing (..)
import StateMachine


{-| Generates an svg of the given siteswap.

    renderExpr <canvas width> <expression>
    renderExpr 500 "3[12][22]"

-}
renderExpr : Int -> String -> Html.Html msg
renderExpr canvas_width input_string =
    let
        parseObject : ParseObject
        parseObject =
            StateMachine.parseExpr input_string

        opts : Opts
        opts =
            { num_tokens = List.length parseObject.tokens
            , canvas = { w = toFloat canvas_width, h = toFloat canvas_width }
            , unit =
                { w = (toFloat canvas_width) / (toFloat <| List.length parseObject.tokens)
                , h = 30.0
                }
            , self_arrow = { w = 25, h = 35 }
            , arrow_offset = { x = 10.0, y = 25.0 }
            , text_offset = { x = -4.0, y = 8.0 }
            , viewbox_offset = { x = -5, y = 0 }
            , y_delt = 15.0
            , is_sync = parseObject.is_sync
            }
    in
        S.svg
            [ SA.width <| toString opts.canvas.w
            , SA.height <| toString opts.canvas.h
            , Draw.mkViewbox opts
            ]
        <|
            [ S.defs [] [ Arrow.definition ]
            , Draw.multiplexBoxes opts parseObject
            , Draw.tokenBoxes opts parseObject
            , Draw.arrows opts parseObject
            ]
