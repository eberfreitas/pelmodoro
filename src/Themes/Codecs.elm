module Themes.Codecs exposing (decodeTheme, encodeTheme)

import Json.Decode as D
import Json.Encode as E
import Themes.Types exposing (Theme(..))
import Tools


themeToEncodedString : Theme -> String
themeToEncodedString theme =
    case theme of
        Tomato ->
            "light"

        NightMood ->
            "dark"

        Gruvbox ->
            "gruvbox"

        Dracula ->
            "dracula"

        Nord ->
            "nord"


themePairs : List ( Theme, String )
themePairs =
    [ Tomato
    , NightMood
    , Gruvbox
    , Dracula
    , Nord
    ]
        |> Tools.makePairs themeToEncodedString


themeFromEncodedString : String -> Maybe Theme
themeFromEncodedString =
    Tools.fromString themePairs


encodeTheme : Theme -> E.Value
encodeTheme theme =
    theme |> themeToEncodedString |> E.string


decodeTheme : D.Decoder Theme
decodeTheme =
    D.string
        |> D.andThen
            (themeFromEncodedString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid theme")
            )
