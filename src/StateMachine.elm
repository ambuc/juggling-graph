module StateMachine exposing (parseExpr, allValid)

import Char
import Debug exposing (crash)
import List.Extra as LE


--

import Lib exposing (..)


{-| Guide to State notation:
S = SYNC
AS = ASYNC
W = WAITING
-}
type State
    = INIT
    | S_NORMAL
    | S_W_FOR_FIRST
    | S_W_FOR_FIRST_BRACE_EMPTY
    | S_W_FOR_FIRST_BRACE
    | S_W_FOR_FIRST_X_OR_BRACE
    | S_W_FOR_FIRST_X
    | S_W_FOR_COMMA
    | S_W_FOR_SECOND
    | S_W_FOR_SECOND_X
    | S_W_FOR_SECOND_BRACE_EMPTY
    | S_W_FOR_SECOND_BRACE
    | S_W_FOR_SECOND_X_OR_BRACE
    | S_W_FOR_PAREN
    | AS_NORMAL
    | AS_W_FOR_BRACE_EMPTY
    | AS_W_FOR_BRACE


emptyChar : Char
emptyChar =
    ' '


emptyThrow : Throw
emptyThrow =
    { value = 0
    , is_cross = False
    , is_valid = True
    , char = emptyChar
    , hand = Center
    }


markAsCross : Throw -> Throw
markAsCross throw =
    { throw | is_cross = True }


markAs : Hand -> Throw -> Throw
markAs hand throw =
    { throw | hand = hand }


toInt : Char -> Maybe Int
toInt c =
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
    { valid = True, throws = [] }


newBeat : Hand -> Char -> Beat
newBeat hand chr =
    case (toInt chr) of
        Just int ->
            { emptyBeat
                | throws = [ { emptyThrow | value = int, char = chr, hand = hand } ]
            }

        Nothing ->
            invalidBeat chr


invalidBeat : Char -> Beat
invalidBeat chr =
    { emptyBeat
        | throws = [ { emptyThrow | value = 0, is_valid = False, char = chr } ]
    }


multiplexPush : Hand -> Char -> Beat -> Beat
multiplexPush hand chr beat =
    case (toInt chr) of
        Just int ->
            { beat
                | throws = { emptyThrow | value = int, char = chr, hand = hand } :: beat.throws
            }

        Nothing ->
            { beat
                | throws =
                    { emptyThrow
                        | value = 0
                        , is_valid = False
                        , char =
                            chr
                        , hand = hand
                    }
                        :: beat.throws
                , valid = False
            }


allValidBeats : List Beat -> Bool
allValidBeats beats =
    case (LE.uncons beats) of
        Just ( head, tail ) ->
            List.all .is_valid head.throws && allValidBeats tail

        _ ->
            True


allValid : BeatMap -> Bool
allValid bs =
    case (LE.uncons bs) of
        Just ( hands, tail ) ->
            List.all .valid hands
                && allValidBeats hands
                && allValid tail

        _ ->
            True



------------
-- MACROS --
------------


push_ : Hand -> Char -> BeatMap -> BeatMap
push_ hand x =
    LE.updateAt 0 (LE.updateAt 0 (multiplexPush hand x))



--------------------
-- PARSING BEGINS --
--------------------


parse : ( State, String, BeatMap ) -> BeatMap
parse ( state, string, bs ) =
    if (String.isEmpty string) then
        bs
    else if (not <| allValid bs) then
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
                                ( AS_W_FOR_BRACE_EMPTY
                                , tail
                                , [ emptyBeat ] :: bs
                                )

                        -- not in a multiplex
                        chr ->
                            parse ( AS_NORMAL, tail, [ newBeat Center chr ] :: bs )

                AS_W_FOR_BRACE_EMPTY ->
                    parse ( AS_W_FOR_BRACE, tail, push_ Center head bs )

                AS_W_FOR_BRACE ->
                    case head of
                        ']' ->
                            parse ( AS_NORMAL, tail, bs )

                        chr ->
                            parse ( AS_W_FOR_BRACE, tail, push_ Center head bs )

                --------------------------------------------------------------
                -- NOW -- ENTERING -- THE -- SYNCHRONOUS -- PARSING -- ZONE --
                --------------------------------------------------------------
                S_NORMAL ->
                    case head of
                        '(' ->
                            parse
                                ( S_W_FOR_FIRST
                                , tail
                                , [ emptyBeat ] :: bs
                                )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_W_FOR_FIRST ->
                    case head of
                        '[' ->
                            parse ( S_W_FOR_FIRST_BRACE_EMPTY, tail, bs )

                        '0' ->
                            parse ( S_W_FOR_COMMA, tail, push_ Left '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse ( S_W_FOR_FIRST_X, tail, push_ Left chr bs )

                S_W_FOR_FIRST_BRACE_EMPTY ->
                    case head of
                        '0' ->
                            parse ( S_W_FOR_FIRST_BRACE, tail, push_ Left '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse
                                ( S_W_FOR_FIRST_X_OR_BRACE
                                , tail
                                , push_ Left head bs
                                )

                S_W_FOR_FIRST_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_W_FOR_FIRST_BRACE, tail, push_ Left '0' bs )

                        ']' ->
                            parse ( S_W_FOR_COMMA, tail, bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse
                                ( S_W_FOR_FIRST_X_OR_BRACE
                                , tail
                                , push_ Left head bs
                                )

                S_W_FOR_FIRST_X_OR_BRACE ->
                    case head of
                        ']' ->
                            parse ( S_W_FOR_COMMA, tail, bs )

                        '0' ->
                            parse ( S_W_FOR_FIRST_BRACE, tail, push_ Left '0' bs )

                        'x' ->
                            parse
                                ( S_W_FOR_FIRST_BRACE
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
                            parse
                                ( S_W_FOR_FIRST_X_OR_BRACE
                                , tail
                                , push_ Left head bs
                                )

                S_W_FOR_FIRST_X ->
                    case head of
                        'x' ->
                            parse
                                ( S_W_FOR_COMMA
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
                            parse ( S_W_FOR_SECOND, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_W_FOR_COMMA ->
                    case head of
                        ',' ->
                            parse ( S_W_FOR_SECOND, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs

                S_W_FOR_SECOND ->
                    case head of
                        '0' ->
                            parse
                                ( S_W_FOR_PAREN
                                , tail
                                , LE.updateAt 0 (\x -> newBeat Right '0' :: x) bs
                                )

                        '[' ->
                            parse
                                ( S_W_FOR_SECOND_BRACE_EMPTY
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
                                ( S_W_FOR_SECOND_X
                                , tail
                                , LE.updateAt 0 (\x -> newBeat Right chr :: x) bs
                                )

                S_W_FOR_SECOND_X ->
                    case head of
                        ')' ->
                            parse ( S_NORMAL, tail, bs )

                        'x' ->
                            parse
                                ( S_W_FOR_PAREN
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

                S_W_FOR_SECOND_BRACE_EMPTY ->
                    case head of
                        '0' ->
                            parse ( S_W_FOR_SECOND_BRACE, tail, push_ Right '0' bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse
                                ( S_W_FOR_SECOND_X_OR_BRACE
                                , tail
                                , push_ Right chr bs
                                )

                S_W_FOR_SECOND_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_W_FOR_SECOND_BRACE, tail, push_ Right '0' bs )

                        ']' ->
                            parse ( S_W_FOR_PAREN, tail, bs )

                        'x' ->
                            [ invalidBeat 'x' ] :: bs

                        chr ->
                            parse
                                ( S_W_FOR_SECOND_X_OR_BRACE
                                , tail
                                , push_ Right chr bs
                                )

                S_W_FOR_SECOND_X_OR_BRACE ->
                    case head of
                        '0' ->
                            parse ( S_W_FOR_SECOND_BRACE, tail, push_ Right '0' bs )

                        'x' ->
                            parse
                                ( S_W_FOR_SECOND_BRACE
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
                            parse ( S_W_FOR_PAREN, tail, bs )

                        chr ->
                            parse
                                ( S_W_FOR_SECOND_X_OR_BRACE
                                , tail
                                , push_ Right chr bs
                                )

                S_W_FOR_PAREN ->
                    case head of
                        ')' ->
                            parse ( S_NORMAL, tail, bs )

                        chr ->
                            [ invalidBeat chr ] :: bs


parseExpr : String -> ParseObject
parseExpr str =
    let
        beatmap =
            List.map (List.map (\x -> { x | throws = List.reverse x.throws })) <|
                List.map List.reverse <|
                    List.reverse <|
                        parse ( INIT, str, [] )
    in
        { beatmap = beatmap
        }
