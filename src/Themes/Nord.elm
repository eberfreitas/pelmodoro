module Themes.Nord exposing (theme)

import Colors exposing (BaseColor)
import Types exposing (ThemeColors)


darkBlue : BaseColor
darkBlue =
    Colors.new 59 66 82 1.0


blue : BaseColor
blue =
    Colors.new 136 192 208 1.0


blue2 : BaseColor
blue2 =
    Colors.new 94 129 172 1.0


blue3 : BaseColor
blue3 =
    Colors.new 76 86 106 1.0


white : BaseColor
white =
    Colors.new 236 239 244 1.0


snow : BaseColor
snow =
    Colors.new 216 222 233 1.0


red : BaseColor
red =
    Colors.new 191 97 106 1.0


theme : ThemeColors
theme =
    ThemeColors snow blue blue2 red darkBlue white blue3
