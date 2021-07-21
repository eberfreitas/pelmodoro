module Theme.Gruvbox exposing (theme)

import Color exposing (Color)
import Theme.Common exposing (ThemeColors)


background : Color
background =
    Color.new 40 40 40 1.0


grey : Color
grey =
    Color.new 146 131 116 1.0


green : Color
green =
    Color.new 152 151 26 1.0


aqua : Color
aqua =
    Color.new 104 157 106 1.0


beige : Color
beige =
    Color.new 235 219 178 1.0


red : Color
red =
    Color.new 250 72 51 1.0


theme : ThemeColors
theme =
    ThemeColors background green aqua red grey beige grey
