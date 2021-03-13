module Colors exposing (..)

import Model exposing (ColorMode, Interval)


intervalToColor : ColorMode -> Interval -> String
intervalToColor mode interval =
    case ( mode, interval ) of
        ( Light, Activity _ ) ->
            "tomato"

        ( Light, Break _ ) ->
            "#2D5BDE"

        ( Light, LongBreak _ ) ->
            "#2DBCE0"

        _ ->
            "TBD"
