module Page.Spotify exposing
    ( Msg
    , State
    , decodeState
    , default
    , encodeState
    , pause
    , play
    , subscriptions
    , update
    , view
    )

import Css
import Elements
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Misc
import Ports
import Theme.Common



-- MODEL-ISH


type State
    = NotConnected String
    | ConnectionError String
    | Connected (List SpotifyPlaylist) (Maybe String)
    | Uninitialized


type alias SpotifyPlaylist =
    ( String, String )



-- VIEW


view : Theme.Common.Theme -> State -> Html.Html Msg
view theme state =
    Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
        [ Html.div [ Attributes.css [ Elements.labelStyle ] ] [ Html.text "Spotify" ]
        , Html.div []
            (case state of
                NotConnected url ->
                    [ Elements.largeLinkButton theme url "Connect to Spotify " ]

                ConnectionError url ->
                    [ Html.p [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ]
                        [ Html.text "There was an error trying to connect. Please, try again!" ]
                    , Elements.largeLinkButton theme url "Connect to Spotify"
                    ]

                Connected playlists current ->
                    [ Html.select [ Attributes.css [ Elements.selectStyle theme ], Events.onInput UpdatePlaylist ]
                        (playlists
                            |> List.sortBy Tuple.second
                            |> List.map
                                (\( uri, title ) ->
                                    Html.option
                                        [ Attributes.value uri, Attributes.selected (current == Just uri) ]
                                        [ Html.text title ]
                                )
                            |> (::) (Html.option [ Attributes.value "" ] [ Html.text "--" ])
                            |> (::)
                                (Html.option
                                    [ Attributes.value "", Attributes.selected (current == Nothing) ]
                                    [ Html.text "Don't play anything" ]
                                )
                        )
                        |> Elements.simpleSeparator
                    , Elements.largeButton theme RefreshPlaylists [ Html.text "Refresh playlists" ]
                        |> Elements.simpleSeparator
                    , Elements.largeButton theme Disconnect [ Html.text "Disconnect" ]
                    ]

                Uninitialized ->
                    [ Html.text "Can't connect to Spotify" ]
            )
        ]



-- UPDATE


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
                |> Misc.addCmd (Refresh |> toPort)

        Disconnect ->
            state
                |> Misc.withCmd
                |> Misc.addCmd (Disconn |> toPort)

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



-- HELPERS


default : State
default =
    Uninitialized


play : State -> Cmd msg
play state =
    case state of
        Connected _ url ->
            url
                |> Maybe.map (Play >> toPort)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


pause : State -> Cmd msg
pause state =
    case state of
        Connected _ _ ->
            Pause |> toPort

        _ ->
            Cmd.none



-- PORTS INTERFACE


type PortAction
    = Play String
    | Pause
    | Refresh
    | Disconn


encodePortAction : PortAction -> Encode.Value
encodePortAction action =
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


toPort : PortAction -> Cmd msg
toPort =
    encodePortAction >> Ports.toSpotify



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.gotFromSpotify GotState



-- CODECS


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
