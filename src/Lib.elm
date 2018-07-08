module Lib exposing (..)

import Char
import Debug as DBG
import Svg as S
import Svg.Attributes as SA


{-| Top-level options struct.
-}
type alias Opts =
    { num_blocks : Int -- number of blocks in the expression
    , cv_w : Int -- the width of the canvas
    , cv_w_2 : Float -- half that
    , unit_w : Float -- the width of a unit block
    , unit_w_2 : Float -- half that
    , unit_h : Float -- the height of a unit block
    , v_off : Float -- the vertical offset of a numeral w/in a block
    , h_off : Float -- the horizontal offset of a numeral w/in a block
    , self_arrow_w : Float -- the width of the self arc
    , self_arrow_h : Float -- the height of the self arc
    , y_delt : Float
    }


type alias Expr =
    String


type alias Coord =
    Float


type alias Index =
    Int


type alias JumpDistance =
    Int


type alias ArrowDescriptor =
    { out_index : Index
    , out_coord : Coord
    , in_index : Index
    , in_coord : Coord
    }



-- VALIDATION


is_valid : Expr -> Bool
is_valid expr =
    List.all (\c -> Char.isDigit c || Char.isLower c || c == ']' || c == '[') <|
        String.toList expr



-- BIAS


type Bias
    = Above
    | Below


bias : ArrowDescriptor -> Bias
bias arr =
    if arr.out_coord < arr.in_coord then
        Above
    else
        Below


{-| Given an expression-scale index, return its x-coordinate position on the
canvas.
-}
toCoordF : Opts -> Float -> Coord
toCoordF os i =
    i * os.unit_w + os.unit_w_2


{-| The same as toCoordF, but for integers.
-}
toCoordI : Opts -> Index -> Coord
toCoordI os i =
    toCoordF os (toFloat i)


{-| Given a character ('0'..'9', 'a'..'z'), convert it to a jump distance
-}
toInt : Char -> JumpDistance
toInt c =
    if Char.isDigit c then
        Result.withDefault 0 <| String.toInt <| String.fromChar c
    else if Char.isLower c then
        Char.toCode c - 97 + 10
    else
        DBG.crash "unimplemented"



-- SHAPES


{-| The svg definition for a standard arrowhead.
-}
arrow_def : S.Svg msg
arrow_def =
    S.marker
        [ SA.id "arrow"
        , SA.markerWidth "10"
        , SA.markerHeight "10"
        , SA.refX "7"
        , SA.refY "3"
        , SA.orient "auto"
        , SA.markerUnits "strokeWidth"
        ]
        [ S.path
            [ SA.d "M0,0 L0,6 L9,3 z"
            , SA.fill "black"
            ]
            []
        ]


{-| Helper function to wrap Svg.rect, but allows specialized input:

    render_rect
      (<rect_center_x>, <rect_center_y>)
      (<rect_width>, <rect_height>)
      <corner-radius> <color> <opacity>

-}
render_rect :
    ( Float, Float )
    -> ( Float, Float )
    -> Float
    -> String
    -> Float
    -> S.Svg msg
render_rect ( cx, cy ) ( w, h ) radius color opac =
    S.rect
        [ SA.x <| toString <| cx - (w / 2.0)
        , SA.y <| toString <| cy - (h / 2.0)
        , SA.width <| toString w
        , SA.height <| toString h
        , SA.rx <| toString radius
        , SA.ry <| toString radius
        , SA.fill color
        , SA.opacity <| toString opac
        ]
        []


{-| Helper function to wrap an svg path string and return an arrow with that
path.
-}
arrow_path : String -> S.Svg msg
arrow_path d_path =
    S.path
        [ SA.d d_path
        , SA.stroke "black"
        , SA.fill "transparent"
        , SA.strokeWidth "1.0"
        , SA.markerEnd "url(#arrow)"
        ]
        []


{-| Returns a standard semicircular throwing arrow.
-}
throw_arrow : Opts -> ArrowDescriptor -> Bool -> S.Svg msg
throw_arrow os arr should_stop_short =
    let
        r =
            abs <| (arr.out_coord - arr.in_coord) / 2.0

        y_base =
            if (bias arr) == Above then
                os.cv_w_2 - os.unit_h
            else
                os.cv_w_2 + os.v_off

        x_to =
            if should_stop_short then
                if (bias arr) == Above then
                    (arr.out_coord + arr.in_coord)
                        / 2.0
                        + (sqrt <| r ^ 2 - os.y_delt ^ 2)
                else
                    (arr.out_coord + arr.in_coord)
                        / 2.0
                        - (sqrt <| r ^ 2 - os.y_delt ^ 2)
            else
                arr.in_coord

        y_to =
            if should_stop_short then
                if (bias arr) == Above then
                    y_base - os.y_delt
                else
                    y_base + os.y_delt
            else
                y_base
    in
        arrow_path <|
            String.concat
                [ "M"
                , toString arr.out_coord
                , " "
                , toString y_base
                , " A "
                , toString r
                , " "
                , toString r
                , " 0 0 1 "
                , toString x_to
                , " "
                , toString y_to
                ]


{-| Returns a specialized identity throwing arrow.
-}
self_arrow : Opts -> Float -> S.Svg msg
self_arrow os x_base =
    let
        y_base =
            os.cv_w_2 - os.unit_h
    in
        arrow_path <|
            String.concat
                [ "M"
                , toString x_base
                , " "
                , toString y_base
                , " C "
                , toString <| x_base - os.self_arrow_w
                , " "
                , toString <| y_base - os.self_arrow_h
                , ", "
                , toString <| x_base + os.self_arrow_w
                , " "
                , toString <| y_base - os.self_arrow_h
                , ", "
                , toString x_base
                , " "
                , toString y_base
                ]
