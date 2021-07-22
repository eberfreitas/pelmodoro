module Theme.Tomato exposing (theme)

import Color
import Theme.Common


tomato : Color.Color
tomato =
    Color.new 255 99 71 1.0


washedTomato : Color.Color
washedTomato =
    Color.new 255 243 240 1.0


white : Color.Color
white =
    Color.new 255 255 255 1.0


darkGrey : Color.Color
darkGrey =
    Color.new 34 34 34 1.0


purple : Color.Color
purple =
    Color.new 130 2 99 1.0


green : Color.Color
green =
    Color.new 122 199 79 1.0


theme : Theme.Common.ThemeColors
theme =
    Theme.Common.ThemeColors washedTomato tomato purple green tomato white darkGrey
