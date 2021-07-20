module Codecs.Encoders exposing (..)

import Helpers
import Json.Encode as E
import Themes.Codecs exposing (encodeTheme)
import Tools
import Types
    exposing
        ( Continuity
        , Current
        , Cycle
        , Interval(..)
        , Notifications
        , Sentiment
        , Settings
        , Sound
        , Spotify(..)
        , SpotifyPlaylist
        )


encodeInterval : Interval -> E.Value
encodeInterval interval =
    case interval of
        Activity s ->
            E.object
                [ ( "type", E.string "activity" )
                , ( "secs", E.int s )
                ]

        Break s ->
            E.object
                [ ( "type", E.string "break" )
                , ( "secs", E.int s )
                ]

        LongBreak s ->
            E.object
                [ ( "type", E.string "longbreak" )
                , ( "secs", E.int s )
                ]


encodeSentiment : Sentiment -> E.Value
encodeSentiment sentiment =
    sentiment |> Tools.sentimentToString |> E.string


encodeCycle : Cycle -> E.Value
encodeCycle { interval, start, end, seconds, sentiment } =
    E.object
        [ ( "interval", encodeInterval interval )
        , ( "start", Helpers.encodeMaybe Helpers.encodePosix start )
        , ( "end", Helpers.encodeMaybe Helpers.encodePosix end )
        , ( "secs", Helpers.encodeMaybe E.int seconds )
        , ( "sentiment", Helpers.encodeMaybe encodeSentiment sentiment )
        ]


encodeCurrent : Current -> E.Value
encodeCurrent { index, cycle, elapsed } =
    E.object
        [ ( "index", E.int index )
        , ( "cycle", encodeCycle cycle )
        , ( "elapsed", E.int elapsed )
        ]


encodeContinuity : Continuity -> E.Value
encodeContinuity cont =
    cont |> Tools.continuityToString |> E.string


encodeSpotifyPlaylist : SpotifyPlaylist -> E.Value
encodeSpotifyPlaylist ( uri, title ) =
    E.object
        [ ( "uri", E.string uri )
        , ( "title", E.string title )
        ]


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
