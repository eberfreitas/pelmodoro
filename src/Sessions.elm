module Sessions exposing
    ( Active
    , Sentiment
    , Session
    , SessionDef
    , Sessions
    , addElapsed
    , buildSessions
    , calculateSentiment
    , decodeActive
    , decodeSession
    , elapsedPct
    , encodeActive
    , encodeSentiment
    , firstSession
    , isAnyBreak
    , isWork
    , logSession
    , negative
    , neutral
    , newActiveSession
    , newSession
    , positive
    , saveActive
    , secondsLeft
    , secondsToDisplay
    , sentimentToDisplay
    , sentimentToIcon
    , sessionChangeToLabel
    , sessionDefToString
    , sessionMaterialized
    , sessionSeconds
    , sessionStart
    , sessionsTotalRun
    , setSessionStart
    , toColor
    )

import Color
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Misc
import Ports
import String.Extra
import Theme.Common
import Theme.Theme as Theme
import Time


type SessionDef
    = Work Int
    | Break Int
    | LongBreak Int


type Sentiment
    = Positive
    | Neutral
    | Negative


type alias Session =
    { def : SessionDef
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    , seconds : Maybe Int
    , sentiment : Maybe Sentiment
    }


type alias Active =
    { index : Int
    , session : Session
    , elapsed : Int
    }


type alias Sessions =
    { active : Active
    , playing : Bool
    , uptime : Int
    , sessions : List SessionDef
    }


sendToLog : Session -> Cmd msg
sendToLog session =
    session |> encodeSession |> Ports.log


saveActive : Active -> Cmd msg
saveActive active =
    active |> encodeActive |> Ports.localStorageHelper "active"


firstSession : List SessionDef -> SessionDef
firstSession =
    List.head >> Maybe.withDefault (Work (25 * 60))


newSession : SessionDef -> Session
newSession def =
    Session def Nothing Nothing Nothing Nothing


newActiveSession : List SessionDef -> Active
newActiveSession sessions =
    Active 0 (newSession (firstSession sessions)) 0


setSessionStart : Time.Posix -> Session -> Session
setSessionStart now session =
    { session | start = Just now }


buildSessions :
    { a | workDuration : Int, breakDuration : Int, longBreakDuration : Int, rounds : Int }
    -> Maybe Active
    -> ( List SessionDef, Active )
buildSessions settings active =
    let
        sessions =
            settings.workDuration
                |> Work
                |> List.repeat settings.rounds
                |> List.intersperse (Break settings.breakDuration)
                |> Misc.flip (++) [ LongBreak settings.longBreakDuration ]

        baseActive =
            newActiveSession sessions

        newActive =
            active
                |> Maybe.map
                    (\({ index, session } as curr) ->
                        case List.Extra.getAt index sessions of
                            Just i ->
                                if i == session.def then
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
    sentimentToString >> String.Extra.toSentenceCase


secondsToDisplay : Int -> String
secondsToDisplay secs =
    let
        pad num =
            num |> String.fromInt |> String.padLeft 2 '0'
    in
    if secs < 60 then
        "0:" ++ pad secs

    else
        let
            min =
                (toFloat secs / 60) |> floor
        in
        String.fromInt min ++ ":" ++ pad (secs - (min * 60))


endSession : Time.Posix -> Active -> Maybe Session
endSession now { session, elapsed } =
    if elapsed /= 0 then
        Just { session | end = Just now, seconds = Just elapsed }

    else
        Nothing


logSession : Time.Posix -> Active -> Cmd msg
logSession now active =
    active
        |> endSession now
        |> Maybe.map sendToLog
        |> Maybe.withDefault Cmd.none


sessionStart : Time.Posix -> Session -> Session
sessionStart now session =
    { session | start = Just now }


sessionMaterialized :
    Session
    ->
        Maybe
            { def : SessionDef
            , start : Time.Posix
            , end : Time.Posix
            , seconds : Int
            }
sessionMaterialized { def, start, end, seconds } =
    ( start, end, seconds )
        |> Misc.maybeTrio
        |> Maybe.map
            (\( start_, end_, secs_ ) ->
                { def = def
                , start = start_
                , end = end_
                , seconds = secs_
                }
            )


sessionSeconds : SessionDef -> Int
sessionSeconds interval =
    case interval of
        Work s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


sessionsTotalRun : List SessionDef -> Int
sessionsTotalRun sessions =
    sessions |> List.foldl (\i t -> i |> sessionSeconds |> (+) t) 0


isWork : SessionDef -> Bool
isWork type_ =
    case type_ of
        Work _ ->
            True

        _ ->
            False


isAnyBreak : SessionDef -> Bool
isAnyBreak type_ =
    case type_ of
        Work _ ->
            False

        _ ->
            True


sessionDefToString : SessionDef -> String
sessionDefToString def =
    case def of
        Work _ ->
            "Work"

        Break _ ->
            "Break"

        LongBreak _ ->
            "Long break"


secondsLeft : Active -> Float
secondsLeft { session, elapsed } =
    sessionSeconds session.def - elapsed |> toFloat


addElapsed : Int -> Active -> Active
addElapsed i active =
    { active | elapsed = active.elapsed + i }


elapsedPct : Active -> Float
elapsedPct { session, elapsed } =
    toFloat elapsed * 100 / (toFloat <| sessionSeconds session.def)


toColor : Theme.Common.Theme -> SessionDef -> Color.Color
toColor theme def =
    case def of
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


sessionChangeToLabel : SessionDef -> SessionDef -> String
sessionChangeToLabel from to =
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


calculateSentiment : List Session -> Sentiment
calculateSentiment =
    List.filter (.def >> isWork)
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



-- CODECS


encodeSessionDef : SessionDef -> Encode.Value
encodeSessionDef def =
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


decodeSessionDef : Decode.Decoder SessionDef
decodeSessionDef =
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


encodeSession : Session -> Encode.Value
encodeSession { def, start, end, seconds, sentiment } =
    Encode.object
        [ ( "interval", encodeSessionDef def )
        , ( "start", Misc.encodeMaybe Misc.encodePosix start )
        , ( "end", Misc.encodeMaybe Misc.encodePosix end )
        , ( "secs", Misc.encodeMaybe Encode.int seconds )
        , ( "sentiment", Misc.encodeMaybe encodeSentiment sentiment )
        ]


decodeSession : Decode.Decoder Session
decodeSession =
    Decode.succeed Session
        |> Pipeline.required "interval" decodeSessionDef
        |> Pipeline.required "start" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "end" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "secs" (Decode.nullable Decode.int)
        |> Pipeline.optional "sentiment" (Decode.nullable decodeSentiment) Nothing


encodeActive : Active -> Encode.Value
encodeActive { index, session, elapsed } =
    Encode.object
        [ ( "index", Encode.int index )
        , ( "cycle", encodeSession session )
        , ( "elapsed", Encode.int elapsed )
        ]


decodeActive : Decode.Decoder Active
decodeActive =
    Decode.succeed Active
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "cycle" decodeSession
        |> Pipeline.required "elapsed" Decode.int
