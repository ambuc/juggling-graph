module StateMachine exposing (parseExpr)

import Char
import Debug exposing (crash)
import List.Extra as LE


--

import Lib exposing (..)
import Tokens


---------------------
-- VALIDATION ZONE --
---------------------


is_beatmap_valid : List (List Beat) -> Bool
is_beatmap_valid bs =
    case (LE.uncons bs) of
        Just ( hands, tail ) ->
            List.all .is_valid hands
                && is_beatmap_valid tail

        _ ->
            True



----------------
-- STATE ZONE --
----------------


{-| Guide to State notation:
S = SYNC
AS = ASYNC
The name of a state is implicilty the thing(s) that state is waiting to receive.
-}
type State
    = INIT
    | S_NORMAL
    | S_FIRST
    | S_FIRST_BRACE_EMPTY
    | S_FIRST_BRACE
    | S_FIRST_X_OR_BRACE
    | S_FIRST_X
    | S_COMMA
    | S_SECOND
    | S_SECOND_X
    | S_SECOND_BRACE_EMPTY
    | S_SECOND_BRACE
    | S_SECOND_X_OR_BRACE
    | S_PAREN
    | AS_NORMAL
    | AS_BRACE_EMPTY
    | AS_BRACE


emptyThrow : Throw
emptyThrow =
    { value = 0
    , is_cross = False
    , is_valid = True
    , char = ' '
    , hand = Center
    }


markAsCross : Throw -> Throw
markAsCross throw =
    { throw | is_cross = True }


charToInt : Char -> Maybe Int
charToInt c =
    if Char.isDigit c then
        case (String.toInt <| String.fromChar c) of
            Ok int ->
                Just int

            _ ->
                Nothing
    else if Char.isLower c then
        Just <| Char.toCode c - 97 + 10
    else
        Nothing


emptyBeat : Beat
emptyBeat =
    { is_valid = True, throws = [] }


newBeat : Hand -> Char -> Beat
newBeat hand chr =
    case (charToInt chr) of
        Just int ->
            { emptyBeat
                | throws = [ { emptyThrow | value = int, char = chr, hand = hand } ]
            }

        Nothing ->
            invalidBeat chr


invalidThrow : Char -> Throw
invalidThrow chr =
    { emptyThrow | value = 0, is_valid = False, char = chr }


invalidBeat : Char -> Beat
invalidBeat chr =
    { emptyBeat
        | throws = [ invalidThrow chr ]
        , is_valid = False
    }


multiplexPush : Hand -> Char -> Beat -> Beat
multiplexPush hand chr beat =
    case (charToInt chr) of
        Just int ->
            { beat | throws = { emptyThrow | value = int, char = chr, hand = hand } :: beat.throws }

        Nothing ->
            { beat
                | throws = invalidThrow chr :: beat.throws
                , is_valid = False
            }



------------
-- MACROS --
------------


push_ : Hand -> Char -> List (List Beat) -> List (List Beat)
push_ hand x =
    LE.updateAt 0 (LE.updateAt 0 (multiplexPush hand x))



--------------------
-- PARSING BEGINS --
--------------------


parse : ( State, String, List (List Beat) ) -> List (List Beat)
parse ( state, string, bs ) =
    if (String.isEmpty string) then
        bs
    else if (not <| is_beatmap_valid bs) then
        bs
    else
        let
            ( head, tail ) =
                case (String.uncons string) of
                    Just ( h, t ) ->
                        ( h, t )

                    _ ->
                        Debug.crash "I thought the string wasn't empty!!"
        in
            case state of
                INIT ->
                    case head of
                        '(' ->
                            parse ( S_NORMAL, string, bs )

                        _ ->
                            parse ( AS_NORMAL, string, bs )

                AS_NORMAL ->
                    case head of
                        -- in a multiplex
                        '[' ->
                            parse
                                ( AS_BRACE_EMPTY
                                , tail
                                , [ emptyBeat ] :: bs
                                )

                        -- not in a multiplex
                        chr ->
                            parse ( AS_NORMAL, tail, [ newBeat Center chr ] :: bs )

                AS_BRACE_EMPTY ->
                    parse
                        ( AS_BRACE
                        , tail
                        , push_ Center head bs
                        )

                AS_BRACE ->
                    case head of
                        ']' ->
                            parse ( AS_NORMAL, tail, bs )

                        chr ->
                            parse
                                ( AS_BRACE
                                , tail
                                , push_ Center head bs
                                )

                --------------------------------------------------------------
                -- NOW -- ENTERING -- THE -- SYNCHRONOUS -- PARSING -- ZONE --
                --------------------------------------------------------------
                S_NORMAL ->
                    case head of
                        '(' ->
                            parse ( S_FIRST, tail, [ emptyBeat ] :: bs )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_FIRST ->
                    case head of
                        '[' ->
                            parse ( S_FIRST_BRACE_EMPTY, tail, bs )

                        '0' ->
                            parse ( S_COMMA, tail, push_ LeftHand '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_FIRST_X, tail, push_ LeftHand chr bs )

                S_FIRST_BRACE_EMPTY ->
                    case head of
                        '0' ->
                            parse ( S_FIRST_BRACE, tail, push_ LeftHand '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_FIRST_X_OR_BRACE, tail, push_ LeftHand head bs )

                S_FIRST_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_FIRST_BRACE, tail, push_ LeftHand '0' bs )

                        ']' ->
                            parse ( S_COMMA, tail, bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_FIRST_X_OR_BRACE, tail, push_ LeftHand head bs )

                S_FIRST_X_OR_BRACE ->
                    case head of
                        ']' ->
                            parse ( S_COMMA, tail, bs )

                        '0' ->
                            parse ( S_FIRST_BRACE, tail, push_ LeftHand '0' bs )

                        'x' ->
                            parse
                                ( S_FIRST_BRACE
                                , tail
                                , LE.updateAt 0
                                    (LE.updateAt 0
                                        (\x ->
                                            { x
                                                | throws =
                                                    LE.updateAt 0
                                                        markAsCross
                                                        x.throws
                                            }
                                        )
                                    )
                                    bs
                                )

                        chr ->
                            parse ( S_FIRST_X_OR_BRACE, tail, push_ LeftHand head bs )

                S_FIRST_X ->
                    case head of
                        'x' ->
                            parse
                                ( S_COMMA
                                , tail
                                , LE.updateAt 0
                                    (LE.updateAt 0
                                        (\x ->
                                            { x
                                                | throws =
                                                    LE.updateAt 0
                                                        markAsCross
                                                        x.throws
                                            }
                                        )
                                    )
                                    bs
                                )

                        ',' ->
                            parse ( S_SECOND, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_COMMA ->
                    case head of
                        ',' ->
                            parse ( S_SECOND, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_SECOND ->
                    case head of
                        '0' ->
                            parse
                                ( S_PAREN
                                , tail
                                , LE.updateAt 0 (\x -> newBeat RightHand '0' :: x) bs
                                )

                        '[' ->
                            parse
                                ( S_SECOND_BRACE_EMPTY
                                , tail
                                , LE.updateAt 0
                                    (\x ->
                                        emptyBeat :: x
                                    )
                                    bs
                                )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse
                                ( S_SECOND_X
                                , tail
                                , LE.updateAt 0 (\x -> newBeat RightHand chr :: x) bs
                                )

                S_SECOND_X ->
                    case head of
                        ')' ->
                            parse ( S_NORMAL, tail, bs )

                        'x' ->
                            parse
                                ( S_PAREN
                                , tail
                                , LE.updateAt 0
                                    (LE.updateAt 0
                                        (\x ->
                                            { x
                                                | throws =
                                                    LE.updateAt
                                                        0
                                                        markAsCross
                                                        x.throws
                                            }
                                        )
                                    )
                                    bs
                                )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_SECOND_BRACE_EMPTY ->
                    case head of
                        '0' ->
                            parse ( S_SECOND_BRACE, tail, push_ RightHand '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_SECOND_X_OR_BRACE, tail, push_ RightHand chr bs )

                S_SECOND_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_SECOND_BRACE, tail, push_ RightHand '0' bs )

                        ']' ->
                            parse ( S_PAREN, tail, bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_SECOND_X_OR_BRACE, tail, push_ RightHand chr bs )

                S_SECOND_X_OR_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_SECOND_BRACE, tail, push_ RightHand '0' bs )

                        'x' ->
                            parse
                                ( S_SECOND_BRACE
                                , tail
                                , LE.updateAt 0
                                    (LE.updateAt 0
                                        (\x ->
                                            { x
                                                | throws =
                                                    LE.updateAt
                                                        0
                                                        markAsCross
                                                        x.throws
                                            }
                                        )
                                    )
                                    bs
                                )

                        ']' ->
                            parse ( S_PAREN, tail, bs )

                        chr ->
                            parse ( S_SECOND_X_OR_BRACE, tail, push_ RightHand chr bs )

                S_PAREN ->
                    case head of
                        ')' ->
                            parse ( S_NORMAL, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs


parseExpr : String -> ParseObject
parseExpr input_string =
    let
        beatmap =
            parse ( INIT, input_string, [] )
                -- reverse beats
                |> List.reverse
                -- reverse hands within beats
                |> List.map List.reverse
                -- reverse throws within hands within beats
                |> List.map (List.map (\x -> { x | throws = List.reverse x.throws }))
    in
        { beatmap = beatmap
        , is_sync = (String.left 1 input_string == "(")
        , tokens = Tokens.mkTokens beatmap
        , is_valid = is_beatmap_valid beatmap
        }



--
