module Colors exposing
    ( backgroundColor
    , contrastColor
    , foregroundColor
    , intervalColor
    , setAlpha
    , textColor
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
    , foreground : BaseColor
    , contrast : BaseColor
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


white : BaseColor
white =
    BaseColor 255 255 255 1.0


lightTheme : ThemeColors
lightTheme =
    ThemeColors washedTomato tomato purpleBlue lightBlue tomato white darkGrey


darkTheme : ThemeColors
darkTheme =
    ThemeColors darkGrey darkPink darkPurple oilBlue lightGrey white lightGrey


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


foregroundColor : Theme -> BaseColor
foregroundColor =
    themeColors >> .foreground


textColor : Theme -> BaseColor
textColor =
    themeColors >> .text


contrastColor : Theme -> BaseColor
contrastColor =
    themeColors >> .contrast


intervalColor : Theme -> Interval -> BaseColor
intervalColor theme interval =
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


setAlpha : Float -> BaseColor -> BaseColor
setAlpha alpha color =
    { color | alpha = alpha }
