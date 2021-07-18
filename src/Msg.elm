module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Date exposing (Date)
import File exposing (File)
import Json.Decode exposing (Value)
import Time exposing (Zone)
import Types exposing (NotificationType)
import Url exposing (Url)


type Msg
    = NoOp
    | Tick Int
    | AdjustTimeZone Zone
    | Play
    | Pause
    | Skip
    | Reset
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
    | GotStatsLogs Value
    | ChangeLogDate Date
    | UrlChanged Url
    | LinkCliked UrlRequest
    | CloseFlashMsg
    | GotFlashMsg Value
    | ToggleNotification NotificationType
    | GotBrowserNotifRes Value
    | RequestDataExport
    | ImportRequest
    | ImportSelect File
    | ImportData String
