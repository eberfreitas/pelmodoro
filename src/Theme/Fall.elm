module Theme.Fall exposing (theme)

import Color
import Theme.Common

-- Orange
work : Color.Color
work =
    Color.new 255 145 10 1.0

-- Dark green
foreground : Color.Color
foreground =
    Color.new 69 106 57 1.0

-- Light peach
background : Color.Color
background =
    Color.new 254 244 230 1

-- White
contrast : Color.Color
contrast =
    Color.new 255 255 255 1.0

-- Blue
text : Color.Color
text =
    Color.new 155 106 108 1.0

-- Pink
break : Color.Color
break =
    Color.new 105 162 176 1.0

-- Bordeaux / brown
longBreak : Color.Color
longBreak =
    Color.new 224 82 99 1.0


theme : Theme.Common.ThemeColors
theme =
    Theme.Common.ThemeColors background work break longBreak foreground contrast text
