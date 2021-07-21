module Theme.Tomato exposing (theme)

import Color exposing (Color)
import Theme.Common exposing (ThemeColors)


tomato : Color
tomato =
    Color.new 255 99 71 1.0


washedTomato : Color
washedTomato =
    Color.new 255 243 240 1.0


white : Color
white =
    Color.new 255 255 255 1.0


darkGrey : Color
darkGrey =
    Color.new 34 34 34 1.0


purple : Color
purple =
    Color.new 130 2 99 1.0


green : Color
green =
    Color.new 122 199 79 1.0


theme : ThemeColors
theme =
    ThemeColors washedTomato tomato purple green tomato white darkGrey
