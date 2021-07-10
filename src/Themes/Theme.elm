module Themes.Theme exposing
    ( backgroundColor
    , contrastColor
    , foregroundColor
    , intervalColor
    , longBreakColor
    , textColor
    )

import Colors exposing (BaseColor)
import Themes.NightMood as NightMood
import Themes.Tomato as Tomato
import Types exposing (Interval(..), Theme(..), ThemeColors)


themeColors : Theme -> ThemeColors
themeColors theme =
    case theme of
        Tomato ->
            Tomato.theme

        NightMood ->
            NightMood.theme


backgroundColor : Theme -> BaseColor
backgroundColor =
    themeColors >> .background


foregroundColor : Theme -> BaseColor
foregroundColor =
    themeColors >> .foreground


textColor : Theme -> BaseColor
textColor =
    themeColors >> .text


contrastColor : Theme -> BaseColor
contrastColor =
    themeColors >> .contrast


longBreakColor : Theme -> BaseColor
longBreakColor =
    themeColors >> .longBreak


intervalColor : Theme -> Interval -> BaseColor
intervalColor theme interval =
    case interval of
        Activity _ ->
            theme |> themeColors |> .activity

        Break _ ->
            theme |> themeColors |> .break

        LongBreak _ ->
            theme |> themeColors |> .longBreak
