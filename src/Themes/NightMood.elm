module Themes.NightMood exposing (theme)

import Colors exposing (BaseColor)
import Types exposing (ThemeColors)


darkGrey : BaseColor
darkGrey =
    Colors.new 34 34 34 1.0


darkPink : BaseColor
darkPink =
    Colors.new 141 48 99 1.0


darkPurple : BaseColor
darkPurple =
    Colors.new 95 46 136 1.0


oilBlue : BaseColor
oilBlue =
    Colors.new 46 117 137 1.0


lighterGrey : BaseColor
lighterGrey =
    Colors.new 90 90 90 1.0


dirtyWhite : BaseColor
dirtyWhite =
    Colors.new 202 202 202 1.0


lightGrey : BaseColor
lightGrey =
    Colors.new 70 70 70 1.0


theme : ThemeColors
theme =
    ThemeColors darkGrey darkPink darkPurple oilBlue lightGrey dirtyWhite lighterGrey
