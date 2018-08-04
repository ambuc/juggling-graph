module Siteswap exposing (renderExpr)

{-| Experimental package for visualizing siteswaps.

This library accepts a plaintext expression and returns a rendered siteswap
notation image of it. In the context of juggling notation, siteswap is a
notation used to encode juggling patterns.


# renderExpr

@docs renderExpr

-}

import Either exposing (Either(Left, Right))
import Html exposing (..)
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))


--

import Arrow
import Draw
import StateMachine
import Types exposing (..)


-----------------------
-- MAGIC NUMBER ZONE --
-----------------------


mkOpts : Int -> Int -> ParseObject -> View -> Opts
mkOpts canvas_width canvas_height parseObject view =
    { num_tokens = List.length parseObject.tokens
    , canvas = { w = toFloat canvas_width, h = toFloat canvas_height }
    , view = view
    , viewbox_offset = { x = -5, y = 0 }
    , view_opts =
        case view of
            Linear ->
                Left
                    -- LinearOpts
                    { unit =
                        { w =
                            (toFloat canvas_width)
                                / (toFloat <| List.length parseObject.tokens)
                        , h = 15.0
                        }
                    , self_arrow = { x = 25, y = 35 }
                    , arrow_offset = { x = 5.0, y = 13.0 }
                    , text_offset = { x = 0.0, y = 4.0 }
                    , y_delt = 15.0
                    }

            Circular ->
                -- CircularOpts
                Right
                    { radius = (toFloat canvas_width) / 3
                    , unit = { w = 20.0, h = 20.0 }
                    , center =
                        { x = (toFloat canvas_width) / 2.0
                        , y = (toFloat canvas_height) / 2.0
                        }
                    , multiplex_offset = { x = 0.0, y = 6.0 }
                    , self_arrow = { x = 25, y = 35 }
                    }
    }


{-| Generates an svg of the given siteswap.

    renderExpr <canvas width> <expression>
    renderExpr 500 "3[12][22]"

-}
renderExpr : Int -> Int -> String -> View -> Html.Html msg
renderExpr canvas_width canvas_height input_string view =
    let
        parseObject : ParseObject
        parseObject =
            StateMachine.parseExpr input_string

        opts : Opts
        opts =
            mkOpts canvas_width canvas_height parseObject view
    in
        S.svg
            [ SA.width "100%"
            , SA.height <| toString opts.canvas.h
            , Draw.mkViewbox opts
            ]
            [ S.defs [] [ Draw.arrowDefinition ]
            , Draw.multiplexBoxes opts parseObject
            , Draw.tokenBoxes opts parseObject
            , Draw.arrows opts parseObject
            ]



--
