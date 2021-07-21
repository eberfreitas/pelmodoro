module Theme.Dracula exposing (theme)

import Color exposing (Color)
import Theme.Common exposing (ThemeColors)


background : Color
background =
    Color.new 40 42 54 1.0


green : Color
green =
    Color.new 80 250 123 1.0


pink : Color
pink =
    Color.new 255 121 198 1.0


purple : Color
purple =
    Color.new 189 147 249 1.0


blue : Color
blue =
    Color.new 68 71 90 1.0


blue2 : Color
blue2 =
    Color.new 98 114 164 1.0


white : Color
white =
    Color.new 248 248 242 1.0


theme : ThemeColors
theme =
    ThemeColors background pink purple green blue white blue2
