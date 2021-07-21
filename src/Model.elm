module Model exposing
    ( Model
    , buildIntervals
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
import Time exposing (Posix, Zone)


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
