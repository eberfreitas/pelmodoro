module Themes.Dracula exposing (theme)

import Colors exposing (BaseColor)
import Themes.Types exposing (ThemeColors)


background : BaseColor
background =
    Colors.new 40 42 54 1.0


green : BaseColor
green =
    Colors.new 80 250 123 1.0


pink : BaseColor
pink =
    Colors.new 255 121 198 1.0


purple : BaseColor
purple =
    Colors.new 189 147 249 1.0


blue : BaseColor
blue =
    Colors.new 68 71 90 1.0


blue2 : BaseColor
blue2 =
    Colors.new 98 114 164 1.0


white : BaseColor
white =
    Colors.new 248 248 242 1.0


theme : ThemeColors
theme =
    ThemeColors background pink purple green blue white blue2
