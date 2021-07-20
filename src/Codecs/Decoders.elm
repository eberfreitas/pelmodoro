module Codecs.Decoders exposing (..)

import Helpers
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Themes.Codecs exposing (decodeTheme)
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
        , Sound(..)
        , Spotify(..)
        , SpotifyPlaylist
        )


decodeInterval : D.Decoder Interval
decodeInterval =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "activity" ->
                        D.map Activity <| D.field "secs" D.int

                    "break" ->
                        D.map Break <| D.field "secs" D.int

                    "longbreak" ->
                        D.map LongBreak <| D.field "secs" D.int

                    _ ->
                        D.fail <| "Can't decode interval of type: " ++ type_
            )


decodeSentiment : D.Decoder Sentiment
decodeSentiment =
    D.string
        |> D.andThen
            (Tools.sentimentFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid sentiment")
            )



-- D.string
--     |> D.andThen
--         (\sentiment ->
--             case sentiment of
--                 "positive" ->
--                     D.succeed Positive
--                 "neutral" ->
--                     D.succeed Neutral
--                 "negative" ->
--                     D.succeed Negative
--                 _ ->
--                     D.fail <| "Can't decode sentiment of type: " ++ sentiment
--         )


decodeCycle : D.Decoder Cycle
decodeCycle =
    D.succeed Cycle
        |> Pipeline.required "interval" decodeInterval
        |> Pipeline.required "start" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "end" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "secs" (D.nullable D.int)
        |> Pipeline.optional "sentiment" (D.nullable decodeSentiment) Nothing


decodeLog : D.Decoder { ts : Int, logs : List Cycle }
decodeLog =
    D.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" D.int
        |> Pipeline.required "logs" (D.list decodeCycle)


decodeCurrent : D.Decoder Current
decodeCurrent =
    D.succeed Current
        |> Pipeline.required "index" D.int
        |> Pipeline.required "cycle" decodeCycle
        |> Pipeline.required "elapsed" D.int


decodeContinuity : D.Decoder Continuity
decodeContinuity =
    D.string
        |> D.andThen
            (Tools.continuityFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid continuity")
            )


decodeSpotifyPlaylist : D.Decoder SpotifyPlaylist
decodeSpotifyPlaylist =
    D.map2 Tuple.pair
        (D.field "uri" D.string)
        (D.field "title" D.string)


decodeSpotify : D.Decoder Spotify
decodeSpotify =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "uninitialized" ->
                        D.succeed Uninitialized

                    "notconnected" ->
                        D.map NotConnected <| D.field "url" D.string

                    "connectionerror" ->
                        D.map ConnectionError <| D.field "url" D.string

                    "connected" ->
                        D.map2 Connected
                            (D.field "playlists" (D.list decodeSpotifyPlaylist))
                            (D.field "playlist" (D.nullable D.string))

                    _ ->
                        D.fail <| "Invalid spotify state of: " ++ type_
            )


decodeNotifications : D.Decoder Notifications
decodeNotifications =
    D.succeed Notifications
        |> Pipeline.required "inapp" D.bool
        |> Pipeline.required "sound" D.bool
        |> Pipeline.required "browser" D.bool


decodeSound : D.Decoder Sound
decodeSound =
    D.string
        |> D.andThen
            (Tools.soundFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid sound")
            )


decodeSettings : D.Decoder Settings
decodeSettings =
    D.succeed Settings
        |> Pipeline.required "rounds" D.int
        |> Pipeline.required "activity" D.int
        |> Pipeline.required "break" D.int
        |> Pipeline.required "longBreak" D.int
        |> Pipeline.required "theme" decodeTheme
        |> Pipeline.required "continuity" decodeContinuity
        |> Pipeline.required "spotify" decodeSpotify
        |> Pipeline.optional "notifications" decodeNotifications Tools.notificationsDefault
        |> Pipeline.optional "sound" decodeSound WindChimes
