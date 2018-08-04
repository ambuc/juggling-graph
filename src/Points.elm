module Points exposing (..)

--

import Types exposing (..)


(+.+) : XY -> XY -> XY
(+.+) a b =
    { x = a.x + b.x, y = a.y + b.y }
infixr 9 +.+


(-.-) : XY -> XY -> XY
(-.-) a b =
    { x = a.x - b.x, y = a.y - b.y }
infixr 9 -.-


(+/+) : XY -> ( Float, Float ) -> XY
(+/+) a ( theta, dist ) =
    { x = a.x + dist * sin theta
    , y = a.y - dist * cos theta
    }
infixr 9 +/+


flipY : XY -> XY
flipY a =
    { x = a.x, y = -1 * a.y }


flipX : XY -> XY
flipX a =
    { x = -1 * a.x, y = a.y }
