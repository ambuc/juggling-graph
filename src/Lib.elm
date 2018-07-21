module Lib exposing (..)

import Char


type alias WH =
    { w : Float, h : Float }


type alias XY =
    { x : Float
    , y : Float
    }


{-| Top-level options struct.
-}
type alias Opts =
    { num_tokens : Int -- number of blocks in the expression
    , canvas : WH -- the width/height of the canvas
    , unit : WH -- the width/height of a token block
    , self_arrow : WH -- the width/height of the self-arc
    , arrow_offset : XY
    , text_offset : XY
    , viewbox_offset : XY
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
    { is_valid : Bool
    , throws : List Throw
    }


type alias Token =
    { txt : String
    , throw : Maybe Throw
    , recv_index : Int
    , is_recv : Bool
    }


type alias ParseObject =
    { beatmap : List (List Beat)
    , is_sync : Bool
    , tokens : List Token
    , is_valid : Bool
    }
