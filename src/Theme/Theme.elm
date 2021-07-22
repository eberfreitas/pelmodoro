module Theme.Theme exposing
    ( backgroundColor
    , breakColor
    , contrastColor
    , decodeTheme
    , encodeTheme
    , foregroundColor
    , longBreakColor
    , textColor
    , themeFromString
    , workColor
    )

import Color exposing (Color)
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Misc
import Theme.Common exposing (Theme(..), ThemeColors)
import Theme.Dracula as Dracula
import Theme.Gruvbox as Gruvbox
import Theme.NightMood as NightMood
import Theme.Nord as Nord
import Theme.Tomato as Tomato


themeColors : Theme -> ThemeColors
themeColors theme =
    case theme of
        Tomato ->
            Tomato.theme

        NightMood ->
            NightMood.theme

        Gruvbox ->
            Gruvbox.theme

        Dracula ->
            Dracula.theme

        Nord ->
            Nord.theme


backgroundColor : Theme -> Color
backgroundColor =
    themeColors >> .background


foregroundColor : Theme -> Color
foregroundColor =
    themeColors >> .foreground


textColor : Theme -> Color
textColor =
    themeColors >> .text


contrastColor : Theme -> Color
contrastColor =
    themeColors >> .contrast


workColor : Theme -> Color
workColor =
    themeColors >> .work


breakColor : Theme -> Color
breakColor =
    themeColors >> .break


longBreakColor : Theme -> Color
longBreakColor =
    themeColors >> .longBreak


themeToString : Theme -> String
themeToString theme =
    case theme of
        Tomato ->
            "Tomato"

        NightMood ->
            "Night Mood"

        Gruvbox ->
            "Gruvbox"

        Dracula ->
            "Dracula"

        Nord ->
            "Nord"


themePairs : List ( Theme, String )
themePairs =
    [ Tomato
    , NightMood
    , Gruvbox
    , Dracula
    , Nord
    ]
        |> Misc.toPairs themeToString


themeFromString : String -> Maybe Theme
themeFromString =
    Misc.fromPairs themePairs



-- CODECS


encodeTheme : Theme -> Value
encodeTheme =
    themeToString >> E.string


decodeTheme : D.Decoder Theme
decodeTheme =
    D.string
        |> D.andThen
            (themeFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid theme")
            )
