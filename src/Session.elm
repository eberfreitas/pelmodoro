module Session exposing
    ( Active
    , Sentiment
    , Session
    , SessionDef
    , addElapsed
    , buildSessions
    , decodeSession
    , encodeActive
    , firstSession
    , isWork
    , logSession
    , newActiveSession
    , newSession
    , rollActiveSession
    , saveActive
    , secondsLeft
    , sendToLog
    , sessionChangeToFlash
    , sessionStart
    )

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Misc
import Page.Flash as Flash
import Page.Settings as Settings
import Ports
import Quotes
import String.Extra
import Time


type alias Seconds =
    Int


type SessionDef
    = Work Seconds
    | Break Seconds
    | LongBreak Seconds


type Sentiment
    = Positive
    | Neutral
    | Negative


type alias Session =
    { def : SessionDef
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    , seconds : Maybe Seconds
    , sentiment : Maybe Sentiment
    }


type alias Active =
    { index : Int
    , session : Session
    , elapsed : Seconds
    }


sessionChangeToFlash : Time.Posix -> SessionDef -> SessionDef -> ( Flash.FlashMsg msg, String )
sessionChangeToFlash now from to =
    case ( from, to ) of
        ( Work _, Break _ ) ->
            ( Flash.new "Time to take a break" (Quotes.randomHtmlQuote now)
            , "Time to take a break"
            )

        ( Break _, Work _ ) ->
            ( Flash.new "Back to work" (Quotes.randomHtmlQuote now)
            , "Back to work"
            )

        ( Work _, LongBreak _ ) ->
            ( Flash.new "Time to relax" (Quotes.randomHtmlQuote now)
            , "Time to relax"
            )

        ( LongBreak _, Work _ ) ->
            ( Flash.new "What is next?" (Quotes.randomHtmlQuote now)
            , "What is next?"
            )

        _ ->
            ( Flash.empty, "" )


rollActiveSession : Time.Posix -> Int -> Settings.Flow -> List SessionDef -> ( Active, Bool )
rollActiveSession now nextIndex flow sessions =
    let
        firstSession_ =
            sessions |> firstSession

        nextActive =
            case sessions |> List.Extra.getAt nextIndex of
                Nothing ->
                    Active 0 (newSession firstSession_) 0

                Just nextSession ->
                    Active nextIndex (newSession nextSession) 0
    in
    if Settings.shouldKeepPlaying nextActive.index flow then
        ( { nextActive | session = setSessionStart now nextActive.session }
        , True
        )

    else
        ( nextActive, False )


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


buildSessions : Settings.Settings -> Maybe Active -> ( List SessionDef, Active )
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
            , seconds : Seconds
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


sessionSeconds : SessionDef -> Seconds
sessionSeconds interval =
    case interval of
        Work s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


sessionsTotalRun : List SessionDef -> Seconds
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


sessionTypeToString : SessionDef -> String
sessionTypeToString def =
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



-- CODECS


encodeSessionDef : SessionDef -> Encode.Value
encodeSessionDef def =
    case def of
        Work s ->
            Encode.object
                [ ( "type", Encode.string "work" )
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
                    "work" ->
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
        [ ( "def", encodeSessionDef def )
        , ( "start", Misc.encodeMaybe Misc.encodePosix start )
        , ( "end", Misc.encodeMaybe Misc.encodePosix end )
        , ( "secs", Misc.encodeMaybe Encode.int seconds )
        , ( "sentiment", Misc.encodeMaybe encodeSentiment sentiment )
        ]


decodeSession : Decode.Decoder Session
decodeSession =
    Decode.succeed Session
        |> Pipeline.required "def" decodeSessionDef
        |> Pipeline.required "start" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "end" (Decode.nullable Misc.decodePosix)
        |> Pipeline.required "secs" (Decode.nullable Decode.int)
        |> Pipeline.optional "sentiment" (Decode.nullable decodeSentiment) Nothing


encodeActive : Active -> Encode.Value
encodeActive { index, session, elapsed } =
    Encode.object
        [ ( "index", Encode.int index )
        , ( "session", encodeSession session )
        , ( "elapsed", Encode.int elapsed )
        ]


decodeActive : Decode.Decoder Active
decodeActive =
    Decode.succeed Active
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "session" decodeSession
        |> Pipeline.required "elapsed" Decode.int
