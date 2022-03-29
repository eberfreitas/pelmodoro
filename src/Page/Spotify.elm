module Page.Spotify exposing
    ( Msg
    , pauseCmd
    , playCmd
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
import Settings
import Theme.Common



-- VIEW


view : Theme.Common.Theme -> Settings.SpotifyState -> Html.Html Msg
view theme state =
    Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
        [ Html.div [ Attributes.css [ Elements.labelStyle ] ] [ Html.text "Spotify" ]
        , Html.div []
            (case state of
                Settings.NotConnected url ->
                    [ Elements.largeLinkButton theme url "Connect to Spotify " ]

                Settings.ConnectionError url ->
                    [ Html.p [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ]
                        [ Html.text "There was an error trying to connect. Please, try again!" ]
                    , Elements.largeLinkButton theme url "Connect to Spotify"
                    ]

                Settings.Connected playlists current ->
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

                Settings.Uninitialized ->
                    [ Html.text "Can't connect to Spotify" ]
            )
        ]



-- UPDATE


type Msg
    = GotState Decode.Value
    | RefreshPlaylists
    | Disconnect
    | UpdatePlaylist String


update : Msg -> Settings.SpotifyState -> ( Settings.SpotifyState, Cmd msg )
update msg state =
    case msg of
        GotState raw ->
            case Decode.decodeValue Settings.decodeSpotifyState raw of
                Ok protoState ->
                    case ( state, protoState ) of
                        ( Settings.Connected _ (Just playlist), Settings.Connected playlists _ ) ->
                            let
                                newPlaylist =
                                    playlists
                                        |> List.Extra.find (Tuple.first >> (==) playlist)
                                        |> Maybe.map Tuple.first
                            in
                            Settings.Connected playlists newPlaylist |> Misc.withCmd

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
                Settings.Connected playlists _ ->
                    playlists
                        |> List.Extra.find (Tuple.first >> (==) playlist)
                        |> Maybe.map Tuple.first
                        |> Settings.Connected playlists
                        |> Misc.withCmd

                _ ->
                    Misc.withCmd state



-- HELPERS


playCmd : Settings.SpotifyState -> Cmd msg
playCmd state =
    case state of
        Settings.Connected _ url ->
            url
                |> Maybe.map (Play >> toPort)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


pauseCmd : Settings.SpotifyState -> Cmd msg
pauseCmd state =
    case state of
        Settings.Connected _ _ ->
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
