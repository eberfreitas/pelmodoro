port module Session exposing (..)

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


encodeSentiment : Sentiment -> Value
encodeSentiment =
    sentimentToString >> E.string


encodeSession : Session -> Value
encodeSession { type_, start, end, seconds, sentiment } =
    E.object
        [ ( "type", encodeSessionType type_ )
        , ( "start", Misc.encodeMaybe Misc.encodePosix start )
        , ( "end", Misc.encodeMaybe Misc.encodePosix end )
        , ( "secs", Misc.encodeMaybe E.int seconds )
        , ( "sentiment", Misc.encodeMaybe encodeSentiment sentiment )
        ]


encodeActive : Active -> Value
encodeActive { index, session, elapsed } =
    E.object
        [ ( "index", E.int index )
        , ( "session", encodeSession session )
        , ( "elapsed", E.int elapsed )
        ]
