module Codecs.Encoders exposing (..)

import Json.Encode as E


encodeSpotify : Spotify -> E.Value
encodeSpotify spotify =
    case spotify of
        NotConnected url ->
            E.object
                [ ( "type", E.string "notconnected" )
                , ( "url", E.string url )
                ]

        ConnectionError url ->
            E.object
                [ ( "type", E.string "connectionerror" )
                , ( "url", E.string url )
                ]

        Connected playlists playlist ->
            E.object
                [ ( "type", E.string "connected" )
                , ( "playlists", E.list encodeSpotifyPlaylist playlists )
                , ( "playlist", Helpers.encodeMaybe E.string playlist )
                ]

        Uninitialized ->
            E.object
                [ ( "type", E.string "uninitialized" ) ]


encodeNotifications : Notifications -> E.Value
encodeNotifications { inApp, sound, browser } =
    E.object
        [ ( "inapp", E.bool inApp )
        , ( "sound", E.bool sound )
        , ( "browser", E.bool browser )
        ]


encodeSound : Sound -> E.Value
encodeSound sound =
    sound |> Tools.soundToString |> E.string


encodeSettings : Settings -> E.Value
encodeSettings { rounds, activity, break, longBreak, theme, continuity, spotify, notifications, sound } =
    E.object
        [ ( "rounds", E.int rounds )
        , ( "activity", E.int activity )
        , ( "break", E.int break )
        , ( "longBreak", E.int longBreak )
        , ( "theme", encodeTheme theme )
        , ( "continuity", encodeContinuity continuity )
        , ( "spotify", encodeSpotify spotify )
        , ( "notifications", encodeNotifications notifications )
        , ( "sound", encodeSound sound )
        ]
