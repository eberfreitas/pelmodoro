module Colors exposing (backgroundColor, intervalToColor, textColor, toCssColor, toRgbaString)

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


lightTheme : ThemeColors
lightTheme =
    ThemeColors washedTomato tomato purpleBlue lightBlue tomato tomato


darkTheme : ThemeColors
darkTheme =
    ThemeColors tomato tomato purpleBlue lightBlue tomato tomato


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


textColor : Theme -> BaseColor
textColor =
    themeColors >> .text


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
