module Theme.Dracula exposing (theme)

import Color
import Theme.Common


background : Color.Color
background =
    Color.new 40 42 54 1.0


green : Color.Color
green =
    Color.new 80 250 123 1.0


pink : Color.Color
pink =
    Color.new 255 121 198 1.0


purple : Color.Color
purple =
    Color.new 189 147 249 1.0


blue : Color.Color
blue =
    Color.new 68 71 90 1.0


blue2 : Color.Color
blue2 =
    Color.new 98 114 164 1.0


white : Color.Color
white =
    Color.new 248 248 242 1.0


theme : Theme.Common.ThemeColors
theme =
    Theme.Common.ThemeColors background pink purple green blue white blue2
