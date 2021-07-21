module Theme.Nord exposing (theme)

import Color exposing (Color)
import Theme.Common exposing (ThemeColors)


darkBlue : Color
darkBlue =
    Color.new 59 66 82 1.0


blue : Color
blue =
    Color.new 136 192 208 1.0


blue2 : Color
blue2 =
    Color.new 94 129 172 1.0


blue3 : Color
blue3 =
    Color.new 76 86 106 1.0


white : Color
white =
    Color.new 236 239 244 1.0


snow : Color
snow =
    Color.new 216 222 233 1.0


red : Color
red =
    Color.new 191 97 106 1.0


theme : ThemeColors
theme =
    ThemeColors snow blue blue2 red darkBlue white blue3
