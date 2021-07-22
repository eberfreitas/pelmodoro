port module Ports exposing
    ( gotBrowserNotificationPermission
    , gotFromLog
    , gotFromSpotify
    , localStorageHelper
    , log
    , notify
    , reqBrowserNotificationPermission
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


port reqBrowserNotificationPermission : Encode.Value -> Cmd msg


port toLog : Encode.Value -> Cmd msg


port toSettings : Encode.Value -> Cmd msg


port toSpotify : Encode.Value -> Cmd msg



-- SUBS


port tick : (Decode.Value -> msg) -> Sub msg


port gotBrowserNotificationPermission : (Decode.Value -> msg) -> Sub msg


port gotFromSpotify : (Decode.Value -> msg) -> Sub msg


port gotFromLog : (Decode.Value -> msg) -> Sub msg
