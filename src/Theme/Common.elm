module Theme.Common exposing (Theme(..), ThemeColors)

import Color exposing (Color)


type Theme
    = Tomato
    | NightMood
    | Gruvbox
    | Dracula
    | Nord
    | Fall


type alias ThemeColors =
    { background : Color
    , work : Color
    , break : Color
    , longBreak : Color
    , foreground : Color
    , contrast : Color
    , text : Color
    }
