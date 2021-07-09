module Colors exposing
    ( BaseColor
    , new
    , setAlpha
    , toCssColor
    , toRgbaString
    )

import Css exposing (Color)
import Helpers


type BaseColor
    = BaseColor
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


new : Int -> Int -> Int -> Float -> BaseColor
new red green blue alpha =
    BaseColor
        { red = normalizeColor red
        , green = normalizeColor green
        , blue = normalizeColor blue
        , alpha = normalizeAlpha alpha
        }


toCssColor : BaseColor -> Color
toCssColor (BaseColor { red, green, blue, alpha }) =
    Css.rgba red green blue alpha


toRgbaString : BaseColor -> String
toRgbaString (BaseColor { red, green, blue, alpha }) =
    [ red, green, blue ]
        |> List.map String.fromInt
        |> String.join ","
        |> Helpers.flip (++) ("," ++ String.fromFloat alpha)
        |> (\c -> "rgba(" ++ c ++ ")")


setAlpha : Float -> BaseColor -> BaseColor
setAlpha alpha (BaseColor color) =
    { color | alpha = alpha } |> BaseColor
