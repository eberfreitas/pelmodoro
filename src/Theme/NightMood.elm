module Theme.NightMood exposing (theme)

import Color exposing (Color)
import Theme.Common exposing (ThemeColors)


darkGrey : Color
darkGrey =
    Color.new 34 34 34 1.0


darkPink : Color
darkPink =
    Color.new 141 48 99 1.0


darkPurple : Color
darkPurple =
    Color.new 95 46 136 1.0


oilBlue : Color
oilBlue =
    Color.new 46 117 137 1.0


lighterGrey : Color
lighterGrey =
    Color.new 90 90 90 1.0


dirtyWhite : Color
dirtyWhite =
    Color.new 202 202 202 1.0


lightGrey : Color
lightGrey =
    Color.new 70 70 70 1.0


theme : ThemeColors
theme =
    ThemeColors darkGrey darkPink darkPurple oilBlue lightGrey dirtyWhite lighterGrey
