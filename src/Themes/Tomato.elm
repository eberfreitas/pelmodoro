module Themes.Tomato exposing (theme)

import Colors exposing (BaseColor)
import Themes.Types exposing (ThemeColors)


tomato : BaseColor
tomato =
    Colors.new 255 99 71 1.0


washedTomato : BaseColor
washedTomato =
    Colors.new 255 243 240 1.0


white : BaseColor
white =
    Colors.new 255 255 255 1.0


darkGrey : BaseColor
darkGrey =
    Colors.new 34 34 34 1.0


purple : BaseColor
purple =
    Colors.new 130 2 99 1.0


green : BaseColor
green =
    Colors.new 122 199 79 1.0


theme : ThemeColors
theme =
    ThemeColors washedTomato tomato purple green tomato white darkGrey
