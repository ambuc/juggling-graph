module Lib exposing (..)

import Char
import Either exposing (Either)


type View
    = Linear
    | Circular


type alias WH =
    { w : Float, h : Float }


type alias XY =
    { x : Float
    , y : Float
    }


type alias LinearOpts =
    { unit : WH -- the width/height of a token block
    , self_arrow : WH -- the width/height of the self-arc
    , arrow_offset : XY
    , text_offset : XY
    , y_delt : Float -- the y-distance to stop short when curtailing an arrow
    }


type alias CircularOpts =
    { radius : Float
    , unit : WH
    , self_arrow : WH -- the width/height of the self-arc
    , center : XY
    , multiplex_offset : XY
    }


{-| Top-level options struct.
-}
type alias Opts =
    { num_tokens : Int -- number of blocks in the expression
    , canvas : WH -- the width/height of the canvas
    , view : View -- whether we are printing in a linear or circular pattern
    , viewbox_offset : XY
    , view_opts : Either LinearOpts CircularOpts
    }



-----------
-- TYPES --
-----------


type Hand
    = LeftHand
    | RightHand
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
    , is_valid : Bool
    , catch_index : Int
    , is_catch : Bool
    }


type alias ParseObject =
    { beatmap : List (List Beat)
    , is_sync : Bool
    , tokens : List Token
    , is_valid : Bool
    }


type alias Arrow =
    { out_index : Int
    , in_index : Int
    , should_curtail_linear : Bool
    }


type Bias
    = Above
    | Below



--
