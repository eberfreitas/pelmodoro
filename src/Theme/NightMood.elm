module Theme.NightMood exposing (theme)

import Color
import Theme.Common


darkGrey : Color.Color
darkGrey =
    Color.new 34 34 34 1.0


darkPink : Color.Color
darkPink =
    Color.new 141 48 99 1.0


darkPurple : Color.Color
darkPurple =
    Color.new 95 46 136 1.0


oilBlue : Color.Color
oilBlue =
    Color.new 46 117 137 1.0


lighterGrey : Color.Color
lighterGrey =
    Color.new 90 90 90 1.0


dirtyWhite : Color.Color
dirtyWhite =
    Color.new 202 202 202 1.0


lightGrey : Color.Color
lightGrey =
    Color.new 70 70 70 1.0


theme : Theme.Common.ThemeColors
theme =
    Theme.Common.ThemeColors darkGrey darkPink darkPurple oilBlue lightGrey dirtyWhite lighterGrey
