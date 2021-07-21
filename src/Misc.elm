module Misc exposing
    ( decodePosix
    , encodeMaybe
    , encodePosix
    , flip
    , fromPairs
    , maybeTrio
    , toPairs
    )

import Json.Decode as D
import Json.Encode as E
import List.Extra as ListEx
import Time exposing (Posix)


toPairs : (a -> String) -> List a -> List ( a, String )
toPairs fn =
    List.map (\a -> ( a, fn a ))


fromPairs : List ( a, String ) -> String -> Maybe a
fromPairs list s =
    list
        |> ListEx.find (Tuple.second >> (==) s)
        |> Maybe.map Tuple.first


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


maybeTrio : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
maybeTrio ( a, b, c ) =
    case ( a, b, c ) of
        ( Just a_, Just b_, Just c_ ) ->
            Just ( a_, b_, c_ )

        _ ->
            Nothing