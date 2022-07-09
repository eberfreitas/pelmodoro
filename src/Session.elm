module Session exposing
    ( ActiveRound
    , Round
    , RoundType(..)
    , Sentiment
    , addElapsed
    , buildRounds
    , calculateSentiment
    , decodeActiveRound
    , decodeRound
    , elapsedPct
    , encodeActiveRound
    , encodeSentiment
    , firstRound
    , isAnyBreak
    , isWork
    , logRound
    , materializedRound
    , negative
    , neutral
    , newActiveRound
    , newRound
    , positive
    , roundChangeToLabel
    , roundSeconds
    , roundStart
    , roundToColor
    , roundToString
    , roundsTotalRun
    , saveActive
    , secondsLeft
    , sentimentToDisplay
    , sentimentToIcon
    , setRoundStart
    )

import Color
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Misc
import Ports
import String.Extra
import Theme
import Theme.Common
import Time



-- type alias Session =
--     { active : ActiveRound
--     , playing : Bool
--     , uptime : Int
--     , rounds : List RoundType
--     }


type RoundType
    = Work Int
    | Break Int
    | LongBreak Int


type Sentiment
    = Positive
    | Neutral
    | Negative


type alias Round =
    { type_ : RoundType
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    , seconds : Maybe Int
    , sentiment : Maybe Sentiment
    }


type alias ActiveRound =
    { index : Int
    , round : Round
    , elapsed : Int
    }


sendToLog : Round -> Cmd msg
sendToLog session =
    session |> encodeRound |> Ports.log


saveActive : ActiveRound -> Cmd msg
saveActive active =
    active |> encodeActiveRound |> Ports.localStorageHelper "active"


firstRound : List RoundType -> RoundType
firstRound =
    List.head >> Maybe.withDefault (Work (25 * 60))


newRound : RoundType -> Round
newRound round =
    Round round Nothing Nothing Nothing Nothing


newActiveRound : List RoundType -> ActiveRound
newActiveRound rounds =
    ActiveRound 0 (newRound (firstRound rounds)) 0


setRoundStart : Time.Posix -> Round -> Round
setRoundStart now round =
    { round | start = Just now }


buildRounds :
    { a | workDuration : Int, breakDuration : Int, longBreakDuration : Int, rounds : Int }
    -> Maybe ActiveRound
    -> ( List RoundType, ActiveRound )
buildRounds settings active =
    let
        rounds : List RoundType
        rounds =
            settings.workDuration
                |> Work
                |> List.repeat settings.rounds
                |> List.intersperse (Break settings.breakDuration)
                |> Misc.flip (++) [ LongBreak settings.longBreakDuration ]

        baseActive : ActiveRound
        baseActive =
            newActiveRound rounds

        newActive : ActiveRound
        newActive =
            active
                |> Maybe.map
                    (\({ index, round } as curr) ->
                        case List.Extra.getAt index rounds of
                            Just i ->
                                if i == round.type_ then
                                    curr

                                else
                                    baseActive

                            Nothing ->
                                baseActive
                    )
                |> Maybe.withDefault baseActive
    in
    ( rounds, newActive )


sentimentToString : Sentiment -> String
sentimentToString sentiment =
    case sentiment of
        Positive ->
            "positive"

        Neutral ->
            "neutral"

        Negative ->
            "negative"


sentimentPairs : List ( Sentiment, String )
sentimentPairs =
    [ Positive
    , Neutral
    , Negative
    ]
        |> Misc.toPairs sentimentToString


sentimentFromString : String -> Maybe Sentiment
sentimentFromString =
    Misc.fromPairs sentimentPairs


sentimentToDisplay : Sentiment -> String
sentimentToDisplay =
    sentimentToString >> String.Extra.toSentenceCase


endRound : Time.Posix -> ActiveRound -> Maybe Round
endRound now { round, elapsed } =
    if elapsed /= 0 then
        Just { round | end = Just now, seconds = Just elapsed }

    else
        Nothing


logRound : Time.Posix -> ActiveRound -> Cmd msg
logRound now active =
    active
        |> endRound now
        |> Maybe.map sendToLog
        |> Maybe.withDefault Cmd.none


roundStart : Time.Posix -> Round -> Round
roundStart now round =
    { round | start = Just now }


materializedRound :
    Round
    ->
        Maybe
            { type_ : RoundType
            , start : Time.Posix
            , end : Time.Posix
            , seconds : Int
            }
materializedRound { type_, start, end, seconds } =
    ( start, end, seconds )
        |> Misc.maybeTrio
        |> Maybe.map
            (\( start_, end_, secs_ ) ->
                { type_ = type_
                , start = start_
                , end = end_
                , seconds = secs_
                }
            )


roundSeconds : RoundType -> Int
roundSeconds interval =
    case interval of
        Work s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


roundsTotalRun : List RoundType -> Int
roundsTotalRun rounds =
    rounds |> List.foldl (\i t -> i |> roundSeconds |> (+) t) 0


isWork : RoundType -> Bool
isWork round =
    case round of
        Work _ ->
            True

        _ ->
            False


isAnyBreak : RoundType -> Bool
isAnyBreak round =
    case round of
        Work _ ->
            False

        _ ->
            True


roundToString : RoundType -> String
roundToString round =
    case round of
        Work _ ->
            "Work"

        Break _ ->
            "Break"

        LongBreak _ ->
            "Long break"


secondsLeft : ActiveRound -> Float
secondsLeft { round, elapsed } =
    roundSeconds round.type_ - elapsed |> toFloat


addElapsed : Int -> ActiveRound -> ActiveRound
addElapsed i active =
    { active | elapsed = active.elapsed + i }


elapsedPct : ActiveRound -> Float
elapsedPct { round, elapsed } =
    toFloat elapsed * 100 / (toFloat <| roundSeconds round.type_)



-- CODECS


encodeRoundType : RoundType -> Encode.Value
encodeRoundType def =
    case def of
        Work s ->
            Encode.object
                [ ( "type", Encode.string "activity" )
                , ( "secs", Encode.int s )
                ]

        Break s ->
            Encode.object
                [ ( "type", Encode.string "break" )
                , ( "secs", Encode.int s )
                ]

        LongBreak s ->
            Encode.object
                [ ( "type", Encode.string "longbreak" )
                , ( "secs", Encode.int s )
                ]


decodeRoundType : Decode.Decoder RoundType
decodeRoundType =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\def ->
                case def of
                    "activity" ->
                        Decode.map Work <| Decode.field "secs" Decode.int

                    "break" ->
                        Decode.map Break <| Decode.field "secs" Decode.int

                    "longbreak" ->
                        Decode.map LongBreak <| Decode.field "secs" Decode.int

                    _ ->
                        Decode.fail <| "Can't decode interval of type: " ++ def
            )


encodeSentiment : Sentiment -> Encode.Value
encodeSentiment =
    sentimentToString >> Encode.string


decodeSentiment : Decode.Decoder Sentiment
decodeSentiment =
    Decode.string
        |> Decode.andThen
            (sentimentFromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid sentiment")
            )


encodeRound : Round -> Encode.Value
encodeRound { type_, start, end, seconds, sentiment } =
    Encode.object
        [ ( "interval", encodeRoundType type_ )
        , ( "start", Misc.encodeMaybe Misc.encodePosix start )
        , ( "end", Misc.encodeMaybe Misc.encodePosix end )
        , ( "secs", Misc.encodeMaybe Encode.int seconds )
        , ( "sentiment", Misc.encodeMaybe encodeSentiment sentiment )
        ]


decodeRound : Decode.Decoder Round
decodeRound =
    Decode.succeed Round
        |> Pipeline.required "interval" decodeRoundType
        |> Pipeline.required "start" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "end" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "secs" (Decode.nullable Decode.int)
        |> Pipeline.optional "sentiment" (Decode.nullable decodeSentiment) Nothing


encodeActiveRound : ActiveRound -> Encode.Value
encodeActiveRound { index, round, elapsed } =
    Encode.object
        [ ( "index", Encode.int index )
        , ( "cycle", encodeRound round )
        , ( "elapsed", Encode.int elapsed )
        ]


decodeActiveRound : Decode.Decoder ActiveRound
decodeActiveRound =
    Decode.succeed ActiveRound
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "cycle" decodeRound
        |> Pipeline.required "elapsed" Decode.int


roundToColor : Theme.Common.Theme -> RoundType -> Color.Color
roundToColor theme round =
    case round of
        Work _ ->
            Theme.workColor theme

        Break _ ->
            Theme.breakColor theme

        LongBreak _ ->
            Theme.longBreakColor theme


positive : Sentiment
positive =
    Positive


neutral : Sentiment
neutral =
    Neutral


negative : Sentiment
negative =
    Negative


roundChangeToLabel : RoundType -> RoundType -> String
roundChangeToLabel from to =
    case ( from, to ) of
        ( Work _, Break _ ) ->
            "Time to take a break"

        ( Break _, Work _ ) ->
            "Back to work"

        ( Work _, LongBreak _ ) ->
            "Time to relax"

        ( LongBreak _, Work _ ) ->
            "What is next?"

        _ ->
            ""


calculateSentiment : List Round -> Sentiment
calculateSentiment =
    List.filter (.type_ >> isWork)
        >> List.map (.sentiment >> Maybe.withDefault Neutral)
        >> List.foldl
            (\sentiment ( pos, neu, neg ) ->
                case sentiment of
                    Positive ->
                        ( pos + 1, neu, neg )

                    Neutral ->
                        ( pos, neu + 1, neg )

                    Negative ->
                        ( pos, neu, neg + 1 )
            )
            ( 0, 0, 0 )
        >> (\( pos, neu, neg ) ->
                if neg >= neu && neg >= pos then
                    Negative

                else if neu >= pos && neu >= neg then
                    Neutral

                else
                    Positive
           )


sentimentToIcon : Sentiment -> String
sentimentToIcon sentiment =
    case sentiment of
        Positive ->
            "sentiment_satisfied"

        Neutral ->
            "sentiment_neutral"

        Negative ->
            "sentiment_dissatisfied"
