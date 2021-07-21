port module Session exposing (Sentiment, Session, decodeSession)

import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E exposing (Value)
import List.Extra as ListEx
import Misc
import Page.Settings exposing (Settings)
import String.Extra exposing (toSentenceCase)
import Time exposing (Posix)


port logSession : Value -> Cmd msg


type alias Seconds =
    Int


type SessionType
    = Work Seconds
    | Break Seconds
    | LongBreak Seconds


type Sentiment
    = Positive
    | Neutral
    | Negative


type alias Session =
    { type_ : SessionType
    , start : Maybe Posix
    , end : Maybe Posix
    , seconds : Maybe Seconds
    , sentiment : Maybe Sentiment
    }


type alias Active =
    { index : Int
    , session : Session
    , elapsed : Seconds
    }


firstSession : List SessionType -> SessionType
firstSession =
    List.head >> Maybe.withDefault (Work (25 * 60))


sessionBuild : SessionType -> Maybe Posix -> Session
sessionBuild type_ start =
    let
        new =
            Session type_ Nothing Nothing Nothing Nothing
    in
    start |> Maybe.map (\s -> { new | start = Just s }) |> Maybe.withDefault new


buildSessions : Settings -> Maybe Active -> ( List SessionType, Active )
buildSessions settings active =
    let
        sessions =
            settings.workDuration
                |> Work
                |> List.repeat settings.rounds
                |> List.intersperse (Break settings.breakDuration)
                |> Misc.flip (++) [ LongBreak settings.longBreakDuration ]

        baseActive =
            Active 0 (sessionBuild (firstSession sessions) Nothing) 0

        newActive =
            active
                |> Maybe.map
                    (\({ index, session } as curr) ->
                        case ListEx.getAt index sessions of
                            Just i ->
                                if i == session.interval then
                                    curr

                                else
                                    baseActive

                            Nothing ->
                                baseActive
                    )
                |> Maybe.withDefault baseActive
    in
    ( sessions, newActive )


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
    sentimentToString >> toSentenceCase


sessionLog : Posix -> Active -> Cmd msg
sessionLog now { session, elapsed } =
    if elapsed /= 0 then
        { session | end = Just now, seconds = Just elapsed }
            |> encodeSession
            |> logSession

    else
        Cmd.none


sessionStart : Posix -> Session -> Session
sessionStart now session =
    { session | start = Just now }


sessionMaterialized : Session -> Maybe { type_ : SessionType, start : Posix, end : Posix, seconds : Seconds }
sessionMaterialized { type_, start, end, seconds } =
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


sessionSeconds : SessionType -> Seconds
sessionSeconds interval =
    case interval of
        Work s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


sessionsTotalRun : List SessionType -> Seconds
sessionsTotalRun sessions =
    sessions |> List.foldl (\i t -> i |> sessionSeconds |> (+) t) 0


isWork : SessionType -> Bool
isWork type_ =
    case type_ of
        Work _ ->
            True

        _ ->
            False


isBreak : SessionType -> Bool
isBreak type_ =
    case type_ of
        Work _ ->
            False

        _ ->
            True


sessionTypeToString : SessionType -> String
sessionTypeToString type_ =
    case type_ of
        Work _ ->
            "Work"

        Break _ ->
            "Break"

        LongBreak _ ->
            "Long break"


secondsLeft : Active -> Float
secondsLeft { session, elapsed } =
    sessionSeconds session.type_ - elapsed |> toFloat


addElapsed : Int -> Active -> Active
addElapsed i active =
    { active | elapsed = active.elapsed + i }


elapsedPct : Active -> Float
elapsedPct { session, elapsed } =
    toFloat elapsed * 100 / (toFloat <| sessionSeconds session.type_)



-- CODECS


encodeSessionType : SessionType -> Value
encodeSessionType type_ =
    case type_ of
        Work s ->
            E.object
                [ ( "type", E.string "work" )
                , ( "secs", E.int s )
                ]

        Break s ->
            E.object
                [ ( "type", E.string "break" )
                , ( "secs", E.int s )
                ]

        LongBreak s ->
            E.object
                [ ( "type", E.string "longbreak" )
                , ( "secs", E.int s )
                ]


decodeSessionType : D.Decoder SessionType
decodeSessionType =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "work" ->
                        D.map Work <| D.field "secs" D.int

                    "break" ->
                        D.map Break <| D.field "secs" D.int

                    "longbreak" ->
                        D.map LongBreak <| D.field "secs" D.int

                    _ ->
                        D.fail <| "Can't decode interval of type: " ++ type_
            )


encodeSentiment : Sentiment -> Value
encodeSentiment =
    sentimentToString >> E.string


decodeSentiment : D.Decoder Sentiment
decodeSentiment =
    D.string
        |> D.andThen
            (sentimentFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid sentiment")
            )


encodeSession : Session -> Value
encodeSession { type_, start, end, seconds, sentiment } =
    E.object
        [ ( "type", encodeSessionType type_ )
        , ( "start", Misc.encodeMaybe Misc.encodePosix start )
        , ( "end", Misc.encodeMaybe Misc.encodePosix end )
        , ( "secs", Misc.encodeMaybe E.int seconds )
        , ( "sentiment", Misc.encodeMaybe encodeSentiment sentiment )
        ]


decodeSession : D.Decoder Session
decodeSession =
    D.succeed Session
        |> Pipeline.required "type" decodeSessionType
        |> Pipeline.required "start" (D.nullable Misc.decodePosix)
        |> Pipeline.required "end" (D.nullable Misc.decodePosix)
        |> Pipeline.required "secs" (D.nullable D.int)
        |> Pipeline.optional "sentiment" (D.nullable decodeSentiment) Nothing


encodeActive : Active -> Value
encodeActive { index, session, elapsed } =
    E.object
        [ ( "index", E.int index )
        , ( "session", encodeSession session )
        , ( "elapsed", E.int elapsed )
        ]


decodeActive : D.Decoder Active
decodeActive =
    D.succeed Active
        |> Pipeline.required "index" D.int
        |> Pipeline.required "session" decodeSession
        |> Pipeline.required "elapsed" D.int
