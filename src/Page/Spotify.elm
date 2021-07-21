module Page.Spotify exposing (Msg, Spotify)

import Json.Decode exposing (Value)
import Json.Encode as E


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
