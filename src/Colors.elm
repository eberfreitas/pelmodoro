module Colors exposing (backgroundColor, intervalToColor)

import Css exposing (Color)
import Model exposing (Interval(..), Theme(..))


type alias ThemeColors =
    { background : Color
    , activity : Color
    , break : Color
    , longBreak : Color
    , controls : Color
    , text : Color
    }


washedTomato : Color
washedTomato =
    Css.rgb 255 234 230


tomato : Color
tomato =
    Css.rgb 255 99 71


purpleBlue : Color
purpleBlue =
    Css.rgb 45 91 222


lightBlue : Color
lightBlue =
    Css.rgb 45 188 224


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


backgroundColor : Theme -> Color
backgroundColor theme =
    theme |> themeColors |> .background


intervalToColor : Theme -> Interval -> Color
intervalToColor theme interval =
    case interval of
        Activity _ ->
            theme |> themeColors |> .activity

        Break _ ->
            theme |> themeColors |> .break

        LongBreak _ ->
            theme |> themeColors |> .longBreak
