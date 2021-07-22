module Misc exposing
    ( addCmd
    , decodePosix
    , encodeMaybe
    , encodePosix
    , flip
    , fromPairs
    , maybeTrio
    , toPairs
    , updateWith
    , withCmd
    )

import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Time


addCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd cmd =
    Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])


withCmd : a -> ( a, Cmd msg )
withCmd a =
    ( a, Cmd.none )


updateWith : (subMsg -> msg) -> ( a, Cmd subMsg ) -> ( a, Cmd msg )
updateWith fn ( a, cmd ) =
    ( a, Cmd.map fn cmd )


toPairs : (a -> String) -> List a -> List ( a, String )
toPairs fn =
    List.map (\a -> ( a, fn a ))


fromPairs : List ( a, String ) -> String -> Maybe a
fromPairs list s =
    list
        |> List.Extra.find (Tuple.second >> (==) s)
        |> Maybe.map Tuple.first


flip : (b -> a -> c) -> a -> b -> c
flip fn a b =
    fn b a


maybeTrio : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
maybeTrio ( a, b, c ) =
    case ( a, b, c ) of
        ( Just a_, Just b_, Just c_ ) ->
            Just ( a_, b_, c_ )

        _ ->
            Nothing


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe fn value =
    case value of
        Just a ->
            fn a

        Nothing ->
            Encode.null


encodePosix : Time.Posix -> Encode.Value
encodePosix =
    Time.posixToMillis >> Encode.int


decodePosix : Decode.Decoder Time.Posix
decodePosix =
    Decode.map Time.millisToPosix Decode.int
