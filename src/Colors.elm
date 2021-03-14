module Colors exposing
    ( backgroundColor
    , controlsColor
    , intervalToColor
    , toCssColor
    , toRgbaString
    )

import Css exposing (Color)
import Helpers
import Model exposing (Interval(..), Theme(..))


type alias BaseColor =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


type alias ThemeColors =
    { background : BaseColor
    , activity : BaseColor
    , break : BaseColor
    , longBreak : BaseColor
    , controls : BaseColor
    , text : BaseColor
    }


washedTomato : BaseColor
washedTomato =
    BaseColor 255 243 240 1.0


tomato : BaseColor
tomato =
    BaseColor 255 99 71 1.0


purpleBlue : BaseColor
purpleBlue =
    BaseColor 45 91 222 1.0


lightBlue : BaseColor
lightBlue =
    BaseColor 45 188 224 1.0


darkGrey : BaseColor
darkGrey =
    BaseColor 34 34 34 1.0


lightGrey : BaseColor
lightGrey =
    BaseColor 62 62 62 1.0


darkPink : BaseColor
darkPink =
    BaseColor 141 48 99 1.0


darkPurple : BaseColor
darkPurple =
    BaseColor 95 46 136 1.0


oilBlue : BaseColor
oilBlue =
    BaseColor 46 117 137 1.0


lightTheme : ThemeColors
lightTheme =
    ThemeColors washedTomato tomato purpleBlue lightBlue tomato tomato


darkTheme : ThemeColors
darkTheme =
    ThemeColors darkGrey darkPink darkPurple oilBlue lightGrey lightGrey


themeColors : Theme -> ThemeColors
themeColors theme =
    case theme of
        LightTheme ->
            lightTheme

        DarkTheme ->
            darkTheme


backgroundColor : Theme -> BaseColor
backgroundColor =
    themeColors >> .background


controlsColor : Theme -> BaseColor
controlsColor =
    themeColors >> .controls


intervalToColor : Theme -> Interval -> BaseColor
intervalToColor theme interval =
    case interval of
        Activity _ ->
            theme |> themeColors |> .activity

        Break _ ->
            theme |> themeColors |> .break

        LongBreak _ ->
            theme |> themeColors |> .longBreak


toCssColor : BaseColor -> Color
toCssColor { red, green, blue, alpha } =
    Css.rgba red green blue alpha


toRgbaString : BaseColor -> String
toRgbaString { red, green, blue, alpha } =
    [ red, green, blue ]
        |> List.map String.fromInt
        |> String.join ","
        |> Helpers.flip (++) ("," ++ String.fromFloat alpha)
        |> (\c -> "rgba(" ++ c ++ ")")
