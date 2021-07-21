port module Ports exposing (localStorageHelper, log, notify, toSpotify)

import Json.Decode as Decode
import Json.Encode as Encode



-- HELPERS


localStorageHelper : String -> Encode.Value -> Cmd msg
localStorageHelper key val =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "data", val )
        ]
        |> localStorage



-- PORTS


port localStorage : Encode.Value -> Cmd msg


port log : Encode.Value -> Cmd msg


port toSpotify : Encode.Value -> Cmd msg


port notify : Encode.Value -> Cmd msg



-- SUBS


port tick : (Decode.Value -> msg) -> Sub msg
