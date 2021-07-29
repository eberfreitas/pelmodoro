module Theme.Theme exposing
    ( backgroundColor
    , breakColor
    , contrastColor
    , decodeTheme
    , encodeTheme
    , foregroundColor
    , longBreakColor
    , textColor
    , themeTypeAndStrings
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
import Theme.Fall as Fall


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

        Theme.Common.Fall ->
            Fall.theme


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


themeTypeAndStrings : Misc.TypeAndStrings Theme.Common.Theme
themeTypeAndStrings =
    [ ( Theme.Common.Tomato, "light", "Tomato" )
    , ( Theme.Common.NightMood, "dark", "Night Mood" )
    , ( Theme.Common.Gruvbox, "gruvbox", "Gruvbox" )
    , ( Theme.Common.Dracula, "dracula", "Dracula" )
    , ( Theme.Common.Nord, "nord", "Nord" )
    , ( Theme.Common.Fall, "fall", "Fall" )
    ]



-- CODECS


encodeTheme : Theme.Common.Theme -> Encode.Value
encodeTheme =
    Misc.typeToEncodable themeTypeAndStrings >> Maybe.withDefault "" >> Encode.string


decodeTheme : Decode.Decoder Theme.Common.Theme
decodeTheme =
    Decode.string
        |> Decode.andThen
            (Misc.encodableToType themeTypeAndStrings
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid theme")
            )
