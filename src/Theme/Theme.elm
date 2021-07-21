module Theme.Theme exposing
    ( backgroundColor
    , contrastColor
    , foregroundColor
    , longBreakColor
    , sessionColor
    , textColor
    )

import Color exposing (Color)
import Misc
import Session exposing (SessionType(..))
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


longBreakColor : Theme -> Color
longBreakColor =
    themeColors >> .longBreak


sessionColor : Theme -> SessionType -> Color
sessionColor theme interval =
    case interval of
        Work _ ->
            theme |> themeColors |> .activity

        Break _ ->
            theme |> themeColors |> .break

        LongBreak _ ->
            theme |> themeColors |> .longBreak


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
