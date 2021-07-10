module Themes.Gruvbox exposing (theme)

import Colors exposing (BaseColor)
import Types exposing (ThemeColors)


background : BaseColor
background =
    Colors.new 40 40 40 1.0


grey : BaseColor
grey =
    Colors.new 146 131 116 1.0


green : BaseColor
green =
    Colors.new 152 151 26 1.0


aqua : BaseColor
aqua =
    Colors.new 104 157 106 1.0


beige : BaseColor
beige =
    Colors.new 235 219 178 1.0


red : BaseColor
red =
    Colors.new 250 72 51 1.0


theme : ThemeColors
theme =
    ThemeColors background green aqua red grey beige grey
