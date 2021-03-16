module Msg exposing (Msg(..))

import Model exposing (Continuity, Page, Theme)
import Time exposing (Posix, Zone)


type Msg
    = NoOp
    | Tick Posix
    | AdjustTimeZone Zone
    | Play
    | Pause
    | Skip
    | Restart
    | SetCont Continuity
    | SetTheme Theme
    | ChangePage Page
    | ChangeRounds Int
    | ChangeActivity Int
    | ChangeBreak Int
    | ChangeLongBreak Int
