module Themes.Types exposing (Theme(..), ThemeColors, themeFromString, themeStringPairs)

import Colors exposing (BaseColor)
import List.Extra as ListEx


type Theme
    = Tomato
    | NightMood
    | Gruvbox
    | Dracula
    | Nord


type alias ThemeColors =
    { background : BaseColor
    , activity : BaseColor
    , break : BaseColor
    , longBreak : BaseColor
    , foreground : BaseColor
    , contrast : BaseColor
    , text : BaseColor
    }


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


themeStringPairs : List ( Theme, String )
themeStringPairs =
    [ Tomato
    , NightMood
    , Gruvbox
    , Dracula
    , Nord
    ]
        |> List.map (\theme -> ( theme, themeToString theme ))


themeFromString : String -> Maybe Theme
themeFromString theme =
    themeStringPairs
        |> ListEx.find (Tuple.second >> (==) theme)
        |> Maybe.map Tuple.first
