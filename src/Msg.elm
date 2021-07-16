module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Date exposing (Date)
import Json.Decode exposing (Value)
import Time exposing (Zone)
import Types exposing (Continuity, NotificationType, Page)
import Url exposing (Url)


type Msg
    = NoOp
    | Tick Int
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
    | ChangeLogDate Date
    | GotNavLogs Value
    | UrlChanged Url
    | LinkCliked UrlRequest
    | CloseFlashMsg
    | GotFlashMsg Value
    | ToggleNotification NotificationType
    | GotBrowserNotifRes Value
