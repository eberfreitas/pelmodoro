module Page.Spotify exposing
    ( Msg
    , State
    , decodeState
    , default
    , encodeState
    , pause
    , play
    , subs
    , update
    )

import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Misc
import Ports


type State
    = NotConnected String
    | ConnectionError String
    | Connected (List SpotifyPlaylist) (Maybe String)
    | Uninitialized


type alias SpotifyPlaylist =
    ( String, String )


type Msg
    = GotState Decode.Value
    | RefreshPlaylists
    | Disconnect
    | UpdatePlaylist String


update : Msg -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        GotState raw ->
            case Decode.decodeValue decodeState raw of
                Ok protoState ->
                    case ( state, protoState ) of
                        ( Connected _ (Just playlist), Connected playlists _ ) ->
                            let
                                newPlaylist =
                                    playlists
                                        |> List.Extra.find (Tuple.first >> (==) playlist)
                                        |> Maybe.map Tuple.first
                            in
                            Connected playlists newPlaylist |> Misc.withCmd

                        _ ->
                            protoState |> Misc.withCmd

                Err _ ->
                    Misc.withCmd state

        RefreshPlaylists ->
            state
                |> Misc.withCmd
                |> Misc.addCmd (Refresh |> encodeAction |> Ports.toSpotify)

        Disconnect ->
            state
                |> Misc.withCmd
                |> Misc.addCmd (Disconn |> encodeAction |> Ports.toSpotify)

        UpdatePlaylist playlist ->
            case state of
                Connected playlists _ ->
                    playlists
                        |> List.Extra.find (Tuple.first >> (==) playlist)
                        |> Maybe.map Tuple.first
                        |> Connected playlists
                        |> Misc.withCmd

                _ ->
                    Misc.withCmd state


subs : Sub Msg
subs =
    Ports.gotFromSpotify GotState


type Action
    = Play String
    | Pause
    | Refresh
    | Disconn


default : State
default =
    Uninitialized


play : State -> Cmd msg
play state =
    case state of
        Connected _ url ->
            url
                |> Maybe.map (Play >> sendToSpotify)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


pause : State -> Cmd msg
pause state =
    case state of
        Connected _ _ ->
            Pause |> sendToSpotify

        _ ->
            Cmd.none


sendToSpotify : Action -> Cmd msg
sendToSpotify action =
    action |> encodeAction |> Ports.toSpotify



-- CODECS


encodeAction : Action -> Encode.Value
encodeAction action =
    case action of
        Play url ->
            Encode.object
                [ ( "type", Encode.string "play" )
                , ( "url", Encode.string url )
                ]

        Pause ->
            Encode.object [ ( "type", Encode.string "pause" ) ]

        Refresh ->
            Encode.object [ ( "type", Encode.string "refresh" ) ]

        Disconn ->
            Encode.object [ ( "type", Encode.string "disconnect" ) ]


encodeSpotifyPlaylist : SpotifyPlaylist -> Encode.Value
encodeSpotifyPlaylist ( uri, title ) =
    Encode.object
        [ ( "uri", Encode.string uri )
        , ( "title", Encode.string title )
        ]


decodeSpotifyPlaylist : Decode.Decoder SpotifyPlaylist
decodeSpotifyPlaylist =
    Decode.map2 Tuple.pair
        (Decode.field "uri" Decode.string)
        (Decode.field "title" Decode.string)


encodeState : State -> Encode.Value
encodeState state =
    case state of
        NotConnected url ->
            Encode.object
                [ ( "type", Encode.string "notconnected" )
                , ( "url", Encode.string url )
                ]

        ConnectionError url ->
            Encode.object
                [ ( "type", Encode.string "connectionerror" )
                , ( "url", Encode.string url )
                ]

        Connected playlists playlist ->
            Encode.object
                [ ( "type", Encode.string "connected" )
                , ( "playlists", Encode.list encodeSpotifyPlaylist playlists )
                , ( "playlist", Misc.encodeMaybe Encode.string playlist )
                ]

        Uninitialized ->
            Encode.object
                [ ( "type", Encode.string "uninitialized" ) ]


decodeState : Decode.Decoder State
decodeState =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "uninitialized" ->
                        Decode.succeed Uninitialized

                    "notconnected" ->
                        Decode.map NotConnected <| Decode.field "url" Decode.string

                    "connectionerror" ->
                        Decode.map ConnectionError <| Decode.field "url" Decode.string

                    "connected" ->
                        Decode.map2 Connected
                            (Decode.field "playlists" (Decode.list decodeSpotifyPlaylist))
                            (Decode.field "playlist" (Decode.nullable Decode.string))

                    _ ->
                        Decode.fail <| "Invalid spotify state of: " ++ type_
            )
