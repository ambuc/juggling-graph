module Arrow exposing (..)

import List.Extra as LE
import Maybe
import Maybe.Extra as ME
import Svg as S
import Svg.Attributes as SA
import Ternary exposing ((?))


--

import Lib exposing (..)
import Tokens


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


mkDescriptors : Opts -> List Token -> List Descriptor
mkDescriptors os tokens =
    let
        max_recv_index =
            Maybe.withDefault 0 <| List.maximum <| List.map .recv_index tokens

        recv_idx_list : List Int
        recv_idx_list =
            LE.unique <|
                List.map .recv_index <|
                    List.filter .is_recv tokens
    in
        List.map
            (\( out_index, token ) ->
                let
                    -- this is the index in the recv list of the token's recv_index
                    out_idx_in_recv =
                        Maybe.withDefault 0 <|
                            LE.findIndex (\x -> x == token.recv_index) recv_idx_list

                    jump_distance : Int
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

                    --if os.is_sync then value // 2 else value
                    in_idx_in_recv =
                        (out_idx_in_recv + jump_distance)
                            % (List.length recv_idx_list)

                    in_idx =
                        Maybe.withDefault 0 <|
                            LE.getAt in_idx_in_recv recv_idx_list
                in
                    { emptyDescriptor
                        | out_index = out_index
                        , out_coord = toFloat out_index * os.unit_w
                        , in_index = in_idx
                        , in_coord = toFloat (in_idx) * os.unit_w
                    }
            )
        <|
            List.filter (\( _, token ) -> ME.isJust token.throw) <|
                List.indexedMap (,) <|
                    tokens


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
        List.map
            (\arr ->
                { arr | should_curtail = (hasConflict arr) ? False <| True }
            )
            arrs



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
        [ S.path
            [ SA.d "M0,0 L0,6 L9,3 z"
            , SA.fill "black"
            ]
            []
        ]


{-| Helper function to wrap an svg path string and return an arrow with that
path.
-}
arrowPathWrapper : String -> S.Svg msg
arrowPathWrapper d_path =
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
toSvg : Opts -> Descriptor -> S.Svg msg
toSvg os arr =
    if arr.out_index == arr.in_index then
        toSvgSelfArrow os arr
    else
        toSvgThrownArrow os arr


{-| Returns a specialized identity throwing arrow.
-}
toSvgSelfArrow : Opts -> Descriptor -> S.Svg msg
toSvgSelfArrow os arr =
    let
        x_base =
            arr.out_coord

        y_base =
            ((toFloat os.cv_w) / 2.0) - os.unit_h
    in
        arrowPathWrapper <|
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


toSvgThrownArrow : Opts -> Descriptor -> S.Svg msg
toSvgThrownArrow os arr =
    let
        r =
            abs <| (arr.out_coord - arr.in_coord) / 2.0

        y_base =
            if (bias arr) == Above then
                ((toFloat os.cv_w) / 2.0) - os.unit_h
            else
                ((toFloat os.cv_w) / 2.0) + (Tuple.second os.arrow_dxy)

        x_to =
            if arr.should_curtail then
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
            if arr.should_curtail then
                ((bias arr) == Above)
                    ? (y_base - os.y_delt)
                <|
                    (y_base + os.y_delt)
            else
                y_base
    in
        arrowPathWrapper <|
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
