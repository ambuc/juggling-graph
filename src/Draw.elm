module Draw exposing (mkViewbox, arrows, tokenBoxes, multiplexBoxes)

import List.Extra as LE
import Maybe.Extra as ME
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))
import Template as T
import Template.Infix exposing ((<%), (%>))


--

import Arrow
import Lib exposing (..)


------------------------------
-- DRAWING HELPER FUNCTIONS --
------------------------------


{-| Helper function to wrap Svg.rect, but allows specialized input:

    rectangle
      (<rect_center_x>, <rect_center_y>)
      (<rect_width>, <rect_height>)
      <corner-radius> <color> <opacity>

-}
rectangle : Opts -> Float -> Float -> S.Svg msg
rectangle { canvas, unit } left_coord right_coord =
    S.rect
        [ SA.x <| toString <| left_coord
        , SA.y <| toString <| (canvas.w / 2.0) - (unit.h / 2.0)
        , SA.width <| toString (right_coord - left_coord)
        , SA.height <| toString <| unit.h
        , SA.fill "lightblue"
        ]
        []



--------------------------------
-- EXPORTED DRAWING FUNCTIONS --
--------------------------------


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


{-| Takes in a parseObject and adds arrow colors, computes conflicts and adds
curtails, and finally converts each arrow into an SVG.
-}
arrows : Opts -> ParseObject -> S.Svg msg
arrows opts { tokens, is_valid } =
    if is_valid then
        S.g [] <|
            List.map (Arrow.toSvgArrow opts) <|
                Arrow.addCurtails <|
                    Arrow.addColors <|
                        Arrow.mkDescriptors opts tokens
    else
        S.g [] []


multiplexBoxes : Opts -> ParseObject -> S.Svg msg
multiplexBoxes opts { tokens } =
    S.g [] <|
        List.map2
            (\l_idx r_idx ->
                rectangle opts
                    (toFloat l_idx * opts.unit.w)
                    (toFloat r_idx * opts.unit.w)
            )
            (LE.findIndices (\x -> x.txt == "[") tokens)
            (LE.findIndices (\x -> x.txt == "]") tokens)


tokenBoxes : Opts -> ParseObject -> S.Svg msg
tokenBoxes { text_offset, unit, canvas } { tokens } =
    S.g [] <|
        List.map
            (\( coord, token ) ->
                S.text_
                    [ SA.x <|
                        toString <|
                            text_offset.x
                                + unit.w
                                * toFloat coord
                    , SA.y <|
                        toString <|
                            text_offset.y
                                + (canvas.w / 2.0)
                    , SA.fontSize <| toString unit.h
                    , SA.fill <|
                        -- is ME.isJust (token.throw) && throw.is_valid
                        -- defaults to True, so black is the default color
                        (ME.unwrap True (.is_valid) (token.throw))
                            ? "black"
                        <|
                            "red"
                    ]
                    [ S.text token.txt ]
            )
        <|
            List.indexedMap (,) tokens
