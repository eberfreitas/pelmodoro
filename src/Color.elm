module Color exposing
    ( Color
    , new
    , setAlpha
    , toCssColor
    , toRgbaString
    )

import Css
import Helpers


type Color
    = Color
        { red : Int
        , green : Int
        , blue : Int
        , alpha : Float
        }


normalizeColor : Int -> Int
normalizeColor color =
    if color < 0 then
        0

    else if color > 255 then
        255

    else
        color


normalizeAlpha : Float -> Float
normalizeAlpha alpha =
    if alpha < 0 then
        0

    else if alpha > 1.0 then
        1.0

    else
        alpha


new : Int -> Int -> Int -> Float -> Color
new red green blue alpha =
    Color
        { red = normalizeColor red
        , green = normalizeColor green
        , blue = normalizeColor blue
        , alpha = normalizeAlpha alpha
        }


toCssColor : Color -> Css.Color
toCssColor (Color { red, green, blue, alpha }) =
    Css.rgba red green blue alpha


toRgbaString : Color -> String
toRgbaString (Color { red, green, blue, alpha }) =
    [ red, green, blue ]
        |> List.map String.fromInt
        |> String.join ","
        |> Helpers.flip (++) ("," ++ String.fromFloat alpha)
        |> (\c -> "rgba(" ++ c ++ ")")


setAlpha : Float -> Color -> Color
setAlpha alpha (Color color) =
    { color | alpha = alpha } |> Color
