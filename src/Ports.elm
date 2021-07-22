port module Ports exposing
    ( fetchLogs
    , gotBrowserNotificationPermission
    , gotFromSpotify
    , localStorageHelper
    , log
    , logImportExport
    , notify
    , reqBrowserNotificationPermission
    , testAlarmSound
    , tick
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


port toSpotify : Encode.Value -> Cmd msg


port notify : Encode.Value -> Cmd msg


port reqBrowserNotificationPermission : Encode.Value -> Cmd msg


port logImportExport : Encode.Value -> Cmd msg


port testAlarmSound : Encode.Value -> Cmd msg


port fetchLogs : Encode.Value -> Cmd msg



-- SUBS


port tick : (Decode.Value -> msg) -> Sub msg


port gotBrowserNotificationPermission : (Decode.Value -> msg) -> Sub msg


port gotFromSpotify : (Decode.Value -> msg) -> Sub msg
