port module Ports exposing
    ( gotFlashMsg
    , gotFromLog
    , gotFromSettings
    , gotFromSpotify
    , localStorageHelper
    , log
    , notify
    , tick
    , toLog
    , toSettings
    , toSpotify
    )

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


port notify : Encode.Value -> Cmd msg


port toLog : Encode.Value -> Cmd msg


port toSettings : Encode.Value -> Cmd msg


port toSpotify : Encode.Value -> Cmd msg



-- SUBS


port tick : (Decode.Value -> msg) -> Sub msg


port gotFromLog : (Decode.Value -> msg) -> Sub msg


port gotFromSettings : (Decode.Value -> msg) -> Sub msg


port gotFromSpotify : (Decode.Value -> msg) -> Sub msg


port gotFlashMsg : (Decode.Value -> msg) -> Sub msg
