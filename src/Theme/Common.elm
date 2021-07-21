module Theme.Common exposing (Theme(..), ThemeColors)

import Color exposing (Color)


type Theme
    = Tomato
    | NightMood
    | Gruvbox
    | Dracula
    | Nord


type alias ThemeColors =
    { background : Color
    , activity : Color
    , break : Color
    , longBreak : Color
    , foreground : Color
    , contrast : Color
    , text : Color
    }
