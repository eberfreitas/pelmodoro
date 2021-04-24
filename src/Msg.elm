module Msg exposing (Msg(..))

import Date exposing (Date)
import Json.Decode exposing (Value)
import Time exposing (Posix, Zone)
import Types exposing (Continuity, Page)


type Msg
    = NoOp
    | Tick Posix
    | AdjustTimeZone Zone
    | Play
    | Pause
    | Skip
    | Reset
    | SetCont Continuity
    | ChangePage Page
    | ChangeRounds Int
    | ChangeActivity Int
    | ChangeBreak Int
    | ChangeLongBreak Int
    | ChangeContinuity String
    | ChangeTheme String
    | ChangePlaylist String
    | GotSpotifyState Value
    | SpotifyRefresh
    | SpotifyDisconnect
    | ChangeNavDate Date
    | GotStatsLogs Value
    | DeleteCycle Int
    | ChangeLogDate Date
    | GotNavLogs Value
