module Page.Spotify exposing (Msg, Spotify)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Misc


type Spotify
    = NotConnected String
    | ConnectionError String
    | Connected (List SpotifyPlaylist) (Maybe String)
    | Uninitialized


type alias SpotifyPlaylist =
    ( String, String )


type Msg
    = GotState Value
    | Refresh
    | Disconnect



-- CODECS


encodeSpotifyPlaylist : SpotifyPlaylist -> Value
encodeSpotifyPlaylist ( uri, title ) =
    E.object
        [ ( "uri", E.string uri )
        , ( "title", E.string title )
        ]


decodeSpotifyPlaylist : D.Decoder SpotifyPlaylist
decodeSpotifyPlaylist =
    D.map2 Tuple.pair
        (D.field "uri" D.string)
        (D.field "title" D.string)


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
                , ( "playlist", Misc.encodeMaybe E.string playlist )
                ]

        Uninitialized ->
            E.object
                [ ( "type", E.string "uninitialized" ) ]


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
