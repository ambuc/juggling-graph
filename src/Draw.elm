module Draw exposing (..)

import List.Extra as LE
import Maybe.Extra as ME
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))


--

import Arrow
import Lib exposing (..)


-------------
-- DRAWING --
-------------


mkViewbox : Opts -> S.Attribute msg
mkViewbox os =
    SA.viewBox
        (toString (Tuple.first os.viewbox_dxy)
            ++ " "
            ++ toString
                (Tuple.second os.viewbox_dxy)
            ++ " "
            ++ toString os.cv_w
            ++ " "
            ++ toString os.cv_w
        )


{-| Generates the arrow set.
-}
arrows : Opts -> ParseObject -> List Token -> S.Svg msg
arrows os parseObject tokens =
    S.g [] <|
        List.map (Arrow.toSvg os) <|
            Arrow.addCurtails <|
                Arrow.addColors <|
                    Arrow.mkDescriptors os tokens


tokens : Opts -> List Token -> S.Svg msg
tokens os tks =
    S.g [] <|
        List.append
            -- Draw boxes first
            (List.map2
                (\l_idx r_idx ->
                    rectangle os
                        (toFloat l_idx * os.unit_w)
                        (toFloat r_idx * os.unit_w)
                )
                -- All indices of left brackets
                (LE.findIndices (\x -> x.txt == "[") tks)
                -- All indices of right brackets
                (LE.findIndices (\x -> x.txt == "]") tks)
            )
            -- Draw characters second
            (List.map2
                (\token coord ->
                    S.text_
                        [ SA.x <|
                            toString <|
                                Tuple.first os.text_dxy
                                    + os.unit_w
                                    * toFloat coord
                        , SA.y <|
                            toString <|
                                Tuple.second os.text_dxy
                                    + (toFloat os.cv_w / 2.0)
                        , SA.fontSize <| toString os.unit_h
                        , SA.fill <|
                            -- is ME.isJust (token.throw) && throw.is_valid
                            -- defaults to True, so black is the default color
                            if (ME.unwrap True (.is_valid) (token.throw)) then
                                "black"
                            else
                                "red"
                        ]
                        [ S.text token.txt ]
                )
                tks
                (List.range 0 (os.num_tokens - 1))
            )


{-| Helper function to wrap Svg.rect, but allows specialized input:

    rectangle
      (<rect_center_x>, <rect_center_y>)
      (<rect_width>, <rect_height>)
      <corner-radius> <color> <opacity>

-}
rectangle : Opts -> Float -> Float -> S.Svg msg
rectangle os left_coord right_coord =
    S.rect
        [ SA.x <| toString <| left_coord
        , SA.y <| toString <| (toFloat os.cv_w / 2.0) - (os.unit_h / 2.0)
        , SA.width <| toString (right_coord - left_coord)
        , SA.height <| toString <| os.unit_h
        , SA.fill "lightblue"
        ]
        []
