module Msg exposing (Msg(..))

import Model exposing (Continuity)
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
