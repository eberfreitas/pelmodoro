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
    | Reset
    | SetCont Continuity
    | ChangePage Page
    | ChangeRounds Int
    | ChangeActivity Int
    | ChangeBreak Int
    | ChangeLongBreak Int
    | ChangeContinuity String
    | ChangeTheme String
