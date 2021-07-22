module Theme.Gruvbox exposing (theme)

import Color
import Theme.Common


background : Color.Color
background =
    Color.new 40 40 40 1.0


grey : Color.Color
grey =
    Color.new 146 131 116 1.0


green : Color.Color
green =
    Color.new 152 151 26 1.0


aqua : Color.Color
aqua =
    Color.new 104 157 106 1.0


beige : Color.Color
beige =
    Color.new 235 219 178 1.0


red : Color.Color
red =
    Color.new 250 72 51 1.0


theme : Theme.Common.ThemeColors
theme =
    Theme.Common.ThemeColors background green aqua red grey beige grey
