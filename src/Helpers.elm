module Helpers exposing (decodePosix, encodeMaybe, encodePosix, flip)

import Json.Decode as D
import Json.Encode as E
import Time exposing (Posix)


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
