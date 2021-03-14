module Model exposing
    ( Continuity(..)
    , Current
    , Interval(..)
    , Model
    , Page(..)
    , Seconds
    , Theme(..)
    , currentAddElapsed
    , currentElapsedPct
    , currentSecondsLeft
    , cycleBuild
    , cycleLog
    , cycleStart
    , decodeCurrent
    , default
    , encodeCurrent
    , firstInverval
    , intervalSeconds
    , intervalsTotalRun
    , mapSettings
    )

import Helpers
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Time exposing (Posix, Zone)


type Page
    = TimerPage
    | SettingsPage
    | StatsPage
    | CreditsPage


type alias Seconds =
    Int


type Theme
    = LightTheme
    | DarkTheme


type alias Settings =
    { rounds : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    , theme : Theme
    , continuity : Continuity
    }


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type alias Cycle =
    { interval : Interval
    , start : Maybe Posix
    , end : Maybe Posix
    , seconds : Maybe Seconds
    }


type alias Current =
    { index : Int
    , cycle : Cycle
    , elapsed : Seconds
    }


type Continuity
    = NoCont
    | SimpleCont
    | FullCont


type alias Log =
    List Cycle


type alias Model =
    { zone : Zone
    , time : Posix
    , page : Page
    , uptime : Int
    , settings : Settings
    , current : Current
    , playing : Bool
    , intervals : List Interval
    , log : Log
    }


defaultSettings : Settings
defaultSettings =
    Settings 4 (25 * 60) (5 * 60) (15 * 60) LightTheme NoCont


buildIntervals : Settings -> List Interval
buildIntervals settings =
    settings.activity
        |> Activity
        |> List.repeat settings.rounds
        |> List.intersperse (Break settings.break)
        |> Helpers.flip (++) [ LongBreak settings.longBreak ]


firstInverval : List Interval -> Interval
firstInverval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


cycleBuild : Interval -> Maybe Posix -> Cycle
cycleBuild interval start =
    let
        new =
            Cycle interval Nothing Nothing Nothing
    in
    start |> Maybe.map (\s -> { new | start = Just s }) |> Maybe.withDefault new


default : Model
default =
    let
        intervals =
            buildIntervals defaultSettings

        firstInterval_ =
            firstInverval intervals

        current =
            Current 0 (cycleBuild firstInterval_ Nothing) 0
    in
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , page = TimerPage
    , uptime = 0
    , settings = defaultSettings
    , current = current
    , playing = False
    , intervals = intervals
    , log = []
    }


cycleLog : Posix -> Current -> Log -> Log
cycleLog now { cycle, elapsed } log =
    if elapsed /= 0 then
        { cycle | end = Just now, seconds = Just elapsed }
            |> List.singleton
            |> (++) log

    else
        log


cycleStart : Posix -> Cycle -> Cycle
cycleStart now cycle =
    { cycle | start = Just now }


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


encodeInterval : Interval -> E.Value
encodeInterval interval =
    case interval of
        Activity s ->
            E.object
                [ ( "type", E.string "activity" )
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


encodeCycle : Cycle -> E.Value
encodeCycle { interval, start, end, seconds } =
    E.object
        [ ( "interval", encodeInterval interval )
        , ( "start", Helpers.encodeMaybe Helpers.encodePosix start )
        , ( "end", Helpers.encodeMaybe Helpers.encodePosix end )
        , ( "secs", Helpers.encodeMaybe E.int seconds )
        ]


encodeCurrent : Current -> E.Value
encodeCurrent { index, cycle, elapsed } =
    E.object
        [ ( "index", E.int index )
        , ( "cycle", encodeCycle cycle )
        , ( "elapsed", E.int elapsed )
        ]


decodeInterval : D.Decoder Interval
decodeInterval =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "activity" ->
                        D.map Activity <| D.field "secs" D.int

                    "break" ->
                        D.map Break <| D.field "secs" D.int

                    "longbreak" ->
                        D.map LongBreak <| D.field "secs" D.int

                    _ ->
                        D.fail <| "Can't decode interval of type: " ++ type_
            )


decodeCycle : D.Decoder Cycle
decodeCycle =
    D.succeed Cycle
        |> Pipeline.required "interval" decodeInterval
        |> Pipeline.required "start" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "end" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "secs" (D.nullable D.int)


decodeCurrent : D.Decoder Current
decodeCurrent =
    D.succeed Current
        |> Pipeline.required "index" D.int
        |> Pipeline.required "cycle" decodeCycle
        |> Pipeline.required "elapsed" D.int
