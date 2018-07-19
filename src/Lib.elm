module Lib exposing (..)

import Char


{-| Top-level options struct.
-}
type alias Opts =
    { num_tokens : Int -- number of blocks in the expression
    , cv_w : Int -- the width of the canvas
    , unit_w : Float -- the width of a unit block
    , unit_h : Float -- the height of a unit block
    , self_arrow_w : Float -- the width of the self arc
    , self_arrow_h : Float -- the height of the self arc
    , arrow_dxy : ( Float, Float )
    , text_dxy : ( Float, Float )
    , viewbox_dxy : ( Float, Float )
    , y_delt : Float -- the y-distance to stop short when curtailing an arrow
    , is_sync : Bool
    }



-----------
-- TYPES --
-----------


type Hand
    = Left
    | Right
    | Center


type alias Throw =
    { value : Int
    , hand : Hand
    , is_cross : Bool
    , is_valid : Bool
    , char : Char
    }


type alias Beat =
    { valid : Bool
    , throws : List Throw
    }


type alias BeatMap =
    List (List Beat)


type alias ParseObject =
    { beatmap : List (List Beat)
    }


type alias Token =
    { txt : String
    , throw : Maybe Throw
    , recv_index : Int
    , is_recv : Bool
    }
