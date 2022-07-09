module Page.Spotify exposing
    ( Msg
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
import Spotify
import Theme.Common


view : Theme.Common.Theme -> Spotify.State -> Html.Html Msg
view theme state =
    Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
        [ Html.div [ Attributes.css [ Elements.labelStyle ] ] [ Html.text "Spotify" ]
        , Html.div []
            (case state of
                Spotify.NotConnected url ->
                    [ Elements.largeLinkButton theme url "Connect to Spotify " ]

                Spotify.ConnectionError url ->
                    [ Html.p [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ]
                        [ Html.text "There was an error trying to connect. Please, try again!" ]
                    , Elements.largeLinkButton theme url "Connect to Spotify"
                    ]

                Spotify.Connected playlists current ->
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

                Spotify.Uninitialized ->
                    [ Html.text "Can't connect to Spotify" ]
            )
        ]



-- UPDATE


type Msg
    = GotState Decode.Value
    | RefreshPlaylists
    | Disconnect
    | UpdatePlaylist String


update : Msg -> Spotify.State -> ( Spotify.State, Cmd msg )
update msg state =
    case msg of
        GotState raw ->
            case Decode.decodeValue Spotify.decodeState raw of
                Ok protoState ->
                    case ( state, protoState ) of
                        ( Spotify.Connected _ (Just playlist), Spotify.Connected playlists _ ) ->
                            let
                                newPlaylist : Maybe String
                                newPlaylist =
                                    playlists
                                        |> List.Extra.find (Tuple.first >> (==) playlist)
                                        |> Maybe.map Tuple.first
                            in
                            Spotify.Connected playlists newPlaylist |> Misc.withCmd

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
                Spotify.Connected playlists _ ->
                    playlists
                        |> List.Extra.find (Tuple.first >> (==) playlist)
                        |> Maybe.map Tuple.first
                        |> Spotify.Connected playlists
                        |> Misc.withCmd

                _ ->
                    Misc.withCmd state



-- HELPERS


play : Spotify.State -> Cmd msg
play state =
    case state of
        Spotify.Connected _ url ->
            url
                |> Maybe.map (Play >> toPort)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


pause : Spotify.State -> Cmd msg
pause state =
    case state of
        Spotify.Connected _ _ ->
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
