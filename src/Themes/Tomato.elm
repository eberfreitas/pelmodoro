module Themes.Tomato exposing (theme)

import Colors exposing (BaseColor)
import Types exposing (ThemeColors)


tomato : BaseColor
tomato =
    Colors.new 255 99 71 1.0


washedTomato : BaseColor
washedTomato =
    Colors.new 255 243 240 1.0


purpleBlue : BaseColor
purpleBlue =
    Colors.new 45 91 222 1.0


lightBlue : BaseColor
lightBlue =
    Colors.new 45 188 224 1.0


white : BaseColor
white =
    Colors.new 255 255 255 1.0


darkGrey : BaseColor
darkGrey =
    Colors.new 34 34 34 1.0


theme : ThemeColors
theme =
    ThemeColors washedTomato tomato purpleBlue lightBlue tomato white darkGrey
