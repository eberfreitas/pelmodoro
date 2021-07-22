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
    , themePairs
    , workColor
    )

import Color
import Json.Decode as Decode
import Json.Encode as Encode
import Misc
import Theme.Common
import Theme.Dracula as Dracula
import Theme.Gruvbox as Gruvbox
import Theme.NightMood as NightMood
import Theme.Nord as Nord
import Theme.Tomato as Tomato


themeColors : Theme.Common.Theme -> Theme.Common.ThemeColors
themeColors theme =
    case theme of
        Theme.Common.Tomato ->
            Tomato.theme

        Theme.Common.NightMood ->
            NightMood.theme

        Theme.Common.Gruvbox ->
            Gruvbox.theme

        Theme.Common.Dracula ->
            Dracula.theme

        Theme.Common.Nord ->
            Nord.theme


backgroundColor : Theme.Common.Theme -> Color.Color
backgroundColor =
    themeColors >> .background


foregroundColor : Theme.Common.Theme -> Color.Color
foregroundColor =
    themeColors >> .foreground


textColor : Theme.Common.Theme -> Color.Color
textColor =
    themeColors >> .text


contrastColor : Theme.Common.Theme -> Color.Color
contrastColor =
    themeColors >> .contrast


workColor : Theme.Common.Theme -> Color.Color
workColor =
    themeColors >> .work


breakColor : Theme.Common.Theme -> Color.Color
breakColor =
    themeColors >> .break


longBreakColor : Theme.Common.Theme -> Color.Color
longBreakColor =
    themeColors >> .longBreak


themeToString : Theme.Common.Theme -> String
themeToString theme =
    case theme of
        Theme.Common.Tomato ->
            "Tomato"

        Theme.Common.NightMood ->
            "Night Mood"

        Theme.Common.Gruvbox ->
            "Gruvbox"

        Theme.Common.Dracula ->
            "Dracula"

        Theme.Common.Nord ->
            "Nord"


themePairs : List ( Theme.Common.Theme, String )
themePairs =
    [ Theme.Common.Tomato
    , Theme.Common.NightMood
    , Theme.Common.Gruvbox
    , Theme.Common.Dracula
    , Theme.Common.Nord
    ]
        |> Misc.toPairs themeToString


themeFromString : String -> Maybe Theme.Common.Theme
themeFromString =
    Misc.fromPairs themePairs



-- CODECS


encodeTheme : Theme.Common.Theme -> Encode.Value
encodeTheme =
    themeToString >> Encode.string


decodeTheme : Decode.Decoder Theme.Common.Theme
decodeTheme =
    Decode.string
        |> Decode.andThen
            (themeFromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid theme")
            )
