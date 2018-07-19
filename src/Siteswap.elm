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
import Tokens


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

        tokens =
            Tokens.mkBeatmap parseObject.beatmap

        num_tokens_ =
            List.length tokens

        os : Opts
        os =
            { num_tokens = num_tokens_
            , cv_w = canvas_width
            , unit_w = (toFloat canvas_width) / (toFloat num_tokens_)
            , unit_h = 30.0
            , self_arrow_w = 25
            , self_arrow_h = 35
            , arrow_dxy = ( 0.0, 25.0 )
            , text_dxy = ( -4.0, 8.0 )
            , viewbox_dxy = ( -5, 0 )
            , y_delt = 5.0
            , is_sync = (String.left 1 input_string == "(")
            }
    in
        S.svg
            [ SA.width <| toString os.cv_w
            , SA.height <| toString os.cv_w
            , Draw.mkViewbox os
            ]
        <|
            [ S.defs [] [ Arrow.definition ]
            , Draw.tokens os tokens
            , if (StateMachine.allValid parseObject.beatmap) then
                Draw.arrows os parseObject tokens
              else
                S.g [] []
            ]
