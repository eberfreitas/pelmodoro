module Model exposing
    ( ColorMode
    , Continuity(..)
    , Current
    , Interval
    , Model
    , Page(..)
    , currentAddElapsed
    , currentElapsedPct
    , currentSecondsLeft
    , cycleBuild
    , cycleLog
    , cycleStart
    , default
    , firstInverval
    )

import Helpers
import Time exposing (Posix, Zone)


type Page
    = TimerPage
    | SettingsPage
    | StatsPage
    | CreditsPage


type alias Seconds =
    Int


type ColorMode
    = Light
    | Dark


type alias Settings =
    { rounds : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    , colorMode : ColorMode
    }


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type Feeling
    = Good
    | Neutral
    | Bad


type alias Cycle =
    { interval : Interval
    , start : Maybe Posix
    , end : Maybe Posix
    , feeling : Maybe Feeling
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
    , continuity : Continuity
    , intervals : List Interval
    , log : Log
    }


defaultSettings : Settings
defaultSettings =
    Settings 4 (25 * 60) (5 * 60) (15 * 60) Light


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
            Cycle interval Nothing Nothing Nothing Nothing
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
    , continuity = NoCont
    , intervals = intervals
    , log = []
    }


cycleLog : Posix -> Current -> Log -> Log
cycleLog now { cycle, elapsed } log =
    { cycle | end = Just now, seconds = Just elapsed }
        |> List.singleton
        |> (++) log


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


currentSecondsLeft : Current -> Float
currentSecondsLeft { cycle, elapsed } =
    intervalSeconds cycle.interval - elapsed |> toFloat


currentAddElapsed : Int -> Current -> Current
currentAddElapsed i current =
    { current | elapsed = current.elapsed + i }


currentElapsedPct : Current -> Float
currentElapsedPct { cycle, elapsed } =
    toFloat elapsed * 100 / (toFloat <| intervalSeconds cycle.interval)
