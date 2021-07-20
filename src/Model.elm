port module Model exposing
    ( Model
    , buildIntervals
    , continuityFromString
    , continuityPairs
    , currentAddElapsed
    , currentElapsedPct
    , currentSecondsLeft
    , cycleBuild
    , cycleLog
    , cycleMaterialized
    , cycleStart
    , default
    , firstInterval
    , intervalIsActivity
    , intervalIsBreak
    , intervalSeconds
    , intervalToString
    , intervalsTotalRun
    , mapSettings
    )

import Browser.Navigation exposing (Key)
import Codecs.Encoders as Encoder
import Helpers
import Json.Encode as E
import List.Extra as ListEx
import Msg exposing (Msg)
import Themes.Types exposing (Theme(..))
import Time exposing (Posix, Zone)
import Tools
import Types
    exposing
        ( Continuity(..)
        , Current
        , Cycle
        , FlashMsg
        , Interval(..)
        , Page(..)
        , Seconds
        , Settings
        , Sound(..)
        , Spotify(..)
        )


port logCycle : E.Value -> Cmd msg


type alias Model =
    { zone : Zone
    , time : Posix
    , key : Key
    , page : Page
    , uptime : Int
    , settings : Settings
    , current : Current
    , playing : Bool
    , intervals : List Interval
    , flash : Maybe (FlashMsg Msg)
    , sentimentCycle : Maybe Cycle
    }


defaultSettings : Settings
defaultSettings =
    Settings
        4
        (25 * 60)
        (5 * 60)
        (15 * 60)
        Tomato
        NoCont
        Uninitialized
        Tools.notificationsDefault
        WindChimes


firstInterval : List Interval -> Interval
firstInterval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


buildIntervals : Settings -> Maybe Current -> ( List Interval, Current )
buildIntervals settings current =
    let
        intervals =
            settings.activity
                |> Activity
                |> List.repeat settings.rounds
                |> List.intersperse (Break settings.break)
                |> Helpers.flip (++) [ LongBreak settings.longBreak ]

        baseCurrent =
            Current 0 (cycleBuild (firstInterval intervals) Nothing) 0

        newCurrent =
            current
                |> Maybe.map
                    (\({ index, cycle } as curr) ->
                        case ListEx.getAt index intervals of
                            Just i ->
                                if i == cycle.interval then
                                    curr

                                else
                                    baseCurrent

                            Nothing ->
                                baseCurrent
                    )
                |> Maybe.withDefault baseCurrent
    in
    ( intervals, newCurrent )


cycleBuild : Interval -> Maybe Posix -> Cycle
cycleBuild interval start =
    let
        new =
            Cycle interval Nothing Nothing Nothing Nothing
    in
    start |> Maybe.map (\s -> { new | start = Just s }) |> Maybe.withDefault new


default : Key -> Model
default key =
    let
        ( intervals, current ) =
            buildIntervals defaultSettings Nothing
    in
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , key = key
    , page = TimerPage
    , uptime = 0
    , settings = defaultSettings
    , current = current
    , playing = False
    , intervals = intervals
    , flash = Nothing
    , sentimentCycle = Nothing
    }


cycleLog : Posix -> Current -> Cmd Msg
cycleLog now { cycle, elapsed } =
    if elapsed /= 0 then
        { cycle | end = Just now, seconds = Just elapsed }
            |> Encoder.encodeCycle
            |> logCycle

    else
        Cmd.none


cycleStart : Posix -> Cycle -> Cycle
cycleStart now cycle =
    { cycle | start = Just now }


cycleMaterialized : Cycle -> Maybe { interval : Interval, start : Posix, end : Posix, seconds : Seconds }
cycleMaterialized { interval, start, end, seconds } =
    ( start, end, seconds )
        |> Helpers.maybeTrio
        |> Maybe.map
            (\( start_, end_, secs_ ) ->
                { interval = interval
                , start = start_
                , end = end_
                , seconds = secs_
                }
            )


intervalSeconds : Interval -> Seconds
intervalSeconds interval =
    case interval of
        Activity s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


intervalsTotalRun : List Interval -> Seconds
intervalsTotalRun intervals =
    intervals |> List.foldl (\i t -> i |> intervalSeconds |> (+) t) 0


intervalIsActivity : Interval -> Bool
intervalIsActivity interval =
    case interval of
        Activity _ ->
            True

        _ ->
            False


intervalIsBreak : Interval -> Bool
intervalIsBreak interval =
    case interval of
        Activity _ ->
            False

        _ ->
            True


intervalToString : Interval -> String
intervalToString interval =
    case interval of
        Activity _ ->
            "Active"

        Break _ ->
            "Break"

        LongBreak _ ->
            "Long break"


currentSecondsLeft : Current -> Float
currentSecondsLeft { cycle, elapsed } =
    intervalSeconds cycle.interval - elapsed |> toFloat


currentAddElapsed : Int -> Current -> Current
currentAddElapsed i current =
    { current | elapsed = current.elapsed + i }


currentElapsedPct : Current -> Float
currentElapsedPct { cycle, elapsed } =
    toFloat elapsed * 100 / (toFloat <| intervalSeconds cycle.interval)


mapSettings : (Settings -> Settings) -> Model -> Model
mapSettings fn model =
    { model | settings = fn model.settings }


continuityPairs : List ( Continuity, String )
continuityPairs =
    [ ( NoCont, "No continuity" )
    , ( SimpleCont, "Simple continuity" )
    , ( FullCont, "Full continuity" )
    ]


continuityFromString : String -> Maybe Continuity
continuityFromString cont =
    continuityPairs
        |> ListEx.find (Tuple.second >> (==) cont)
        |> Maybe.map Tuple.first
