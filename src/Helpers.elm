module Helpers exposing (decodePosix, encodeMaybe, encodePosix, flip, icon)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E
import Time exposing (Posix)


icon : String -> Html msg
icon desc =
    Html.span [ HtmlAttr.class "material-icons-round" ] [ Html.text desc ]


flip : (b -> a -> c) -> a -> b -> c
flip fn a b =
    fn b a


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe fn value =
    case value of
        Just a ->
            fn a

        Nothing ->
            E.null


encodePosix : Posix -> E.Value
encodePosix =
    Time.posixToMillis >> E.int


decodePosix : D.Decoder Posix
decodePosix =
    D.map Time.millisToPosix D.int
