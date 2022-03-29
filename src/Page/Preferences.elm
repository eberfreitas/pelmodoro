module Page.Preferences exposing
    ( Model
    , Msg
    , new
    , subscriptions
    , update
    , view
    )

import Css
import Elements
import Env
import File
import File.Select as Select
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Misc
import Page.Flash as Flash
import Page.MiniTimer as MiniTimer
import Page.Spotify as Spotify
import Ports
import Sessions
import Settings
import Task
import Theme.Theme as Theme
import Tuple.Trio as Trio



-- MODEL


type alias Model =
    { env : Env.Env
    , settings : Settings.Settings
    , sessions : Sessions.Sessions
    , flash : Flash.Flash
    }


new : Env.Env -> Settings.Settings -> Sessions.Sessions -> Flash.Flash -> Model
new =
    Model



-- VIEW


view : Model -> Html.Html Msg
view ({ settings } as model) =
    let
        inMinutes seconds =
            seconds // 60
    in
    Html.div []
        [ MiniTimer.view model
        , Html.div
            [ Attributes.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.width <| Css.px 280
                ]
            ]
            [ Elements.h1 settings.theme "Settings"
            , Elements.inputContainer "Rounds" <|
                Elements.numberInput settings.theme 1 8 UpdateRounds settings.rounds
            , Elements.inputContainer "Session duration" <|
                Elements.numberInput settings.theme 1 60 UpdateWorkDuration <|
                    inMinutes settings.workDuration
            , Elements.inputContainer "Break duration" <|
                Elements.numberInput settings.theme 1 60 UpdateBreakDuration <|
                    inMinutes settings.breakDuration
            , Elements.inputContainer "Long break duration" <|
                Elements.numberInput settings.theme 1 60 UpdateLongBreakDuration <|
                    inMinutes settings.longBreakDuration
            , Elements.inputContainer "Rounds flow" <|
                Elements.selectInput settings.theme
                    (Trio.first >> (==) settings.flow)
                    UpdateFlow
                    Settings.flowTypeAndStrings
            , Elements.inputContainer "Notifications"
                ([ ( settings.notifications.inApp, Settings.InApp, "In app messages" )
                 , ( settings.notifications.alarmSound, Settings.AlarmSound, "Play sounds" )
                 , ( settings.notifications.browser, Settings.Browser, "Browser notification" )
                 ]
                    |> List.map (\( v, t, l ) -> Elements.checkbox settings.theme v (ToggleNotification t) l)
                    |> Html.div []
                )
            , if settings.notifications.alarmSound then
                Elements.inputContainer "Alarm sound" <|
                    Html.div []
                        [ Elements.selectInput settings.theme
                            (Trio.first >> (==) settings.alarmSound)
                            UpdateAlarmSound
                            Settings.alarmSoundTypeAndStrings
                            |> Elements.simpleSeparator
                        , Elements.largeButton settings.theme
                            (TestAlarmSound settings.alarmSound)
                            [ Elements.styledIcon [ Css.verticalAlign Css.middle ] "play_arrow" ]
                        ]

              else
                Html.text ""
            , Elements.inputContainer "Color theme" <|
                Elements.selectInput settings.theme
                    (Trio.first >> (==) settings.theme)
                    UpdateTheme
                    Theme.themeTypeAndStrings
            , Spotify.view settings.theme settings.spotify |> Html.map Spotify
            , Elements.inputContainer "Import / Export" <|
                Html.div []
                    [ Elements.largeButton settings.theme ExportRequest [ Html.text "Export" ]
                        |> Elements.simpleSeparator
                    , Elements.largeButton settings.theme ImportRequest [ Html.text "Import" ]
                        |> Elements.simpleSeparator
                    , Elements.largeButton settings.theme ClearLogs [ Html.text "Clear logs" ]
                    ]
            ]
        ]



-- UPDATE


type Msg
    = UpdateRounds Int
    | UpdateWorkDuration Int
    | UpdateBreakDuration Int
    | UpdateLongBreakDuration Int
    | UpdateFlow String
    | UpdateTheme String
    | UpdateAlarmSound String
    | ToggleNotification Settings.NotificationType
    | GotBrowserNotificationPermission Decode.Value
    | ExportRequest
    | ImportRequest
    | ImportSelect File.File
    | ClearLogs
    | ImportData String
    | TestAlarmSound Settings.AlarmSound
    | Spotify Spotify.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        UpdateRounds rounds ->
            model
                |> mapSettings (\s -> { s | rounds = rounds })
                |> Misc.withCmd
                |> save

        UpdateWorkDuration secs ->
            model
                |> mapSettings (\s -> { s | workDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateBreakDuration secs ->
            model
                |> mapSettings (\s -> { s | breakDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateLongBreakDuration secs ->
            model
                |> mapSettings (\s -> { s | longBreakDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateFlow flow ->
            model
                |> mapSettings
                    (\s ->
                        flow
                            |> Misc.encodableToType Settings.flowTypeAndStrings
                            |> Maybe.map (\f -> { s | flow = f })
                            |> Maybe.withDefault s
                    )
                |> Misc.withCmd
                |> save

        UpdateTheme theme ->
            model
                |> mapSettings
                    (\s ->
                        theme
                            |> Misc.encodableToType Theme.themeTypeAndStrings
                            |> Maybe.map (\t -> { s | theme = t })
                            |> Maybe.withDefault s
                    )
                |> Misc.withCmd
                |> save

        UpdateAlarmSound alarm ->
            model
                |> mapSettings
                    (\s ->
                        alarm
                            |> Misc.encodableToType Settings.alarmSoundTypeAndStrings
                            |> Maybe.map (\a -> { s | alarmSound = a })
                            |> Maybe.withDefault s
                    )
                |> Misc.withCmd
                |> save

        ToggleNotification type_ ->
            case type_ of
                Settings.Browser ->
                    model
                        |> Misc.withCmd
                        |> Misc.addCmd
                            (settings.notifications.browser
                                |> not
                                |> RequestBrowserPermission
                                |> toPort
                            )

                _ ->
                    model
                        |> mapSettings (\s -> { s | notifications = Settings.toggleNotification type_ s.notifications })
                        |> Misc.withCmd
                        |> save

        GotBrowserNotificationPermission raw ->
            case Decode.decodeValue decodeBrowserNotificationPermission raw of
                Ok res ->
                    let
                        notifications =
                            settings.notifications

                        newNotifications =
                            { notifications | browser = res.val }

                        flashMsg =
                            if res.msg /= "" then
                                Flash.new "Attention" (Html.div [] [ Html.text res.msg ]) |> Just

                            else
                                Nothing
                    in
                    model
                        |> mapSettings (\s -> { s | notifications = newNotifications })
                        |> Flash.setFlash flashMsg
                        |> Misc.withCmd
                        |> save

                Err _ ->
                    Misc.withCmd model

        ExportRequest ->
            model
                |> Misc.withCmd
                |> Misc.addCmd (RequestExport |> toPort)

        ImportRequest ->
            model
                |> Misc.withCmd
                |> Misc.addCmd (Select.file [ "application/json" ] ImportSelect)

        ImportSelect file ->
            model
                |> Misc.withCmd
                |> Misc.addCmd (Task.perform ImportData (File.toString file))

        ClearLogs ->
            model
                |> Misc.withCmd
                |> Misc.addCmd (Delete |> toPort)

        ImportData data ->
            model
                |> Misc.withCmd
                |> Misc.addCmd (Import data |> toPort)

        TestAlarmSound alarmSound ->
            model
                |> Misc.withCmd
                |> Misc.addCmd
                    (alarmSound
                        |> Misc.typeToEncodable Settings.alarmSoundTypeAndStrings
                        |> Maybe.withDefault ""
                        |> TestAlarm
                        |> toPort
                    )

        Spotify subMsg ->
            Spotify.update subMsg settings.spotify
                |> Tuple.mapFirst (\state -> model |> mapSettings (\s -> { s | spotify = state }))
                |> Misc.updateWith Spotify
                |> save



-- HELPERS


mapSettings : (Settings.Settings -> Settings.Settings) -> Model -> Model
mapSettings fn model =
    { model | settings = fn model.settings }


save : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
save ( { sessions } as model, cmd ) =
    let
        ( newSessions, newActive ) =
            Sessions.buildSessions model.settings (Just model.sessions.active)

        newSessions_ =
            { sessions | sessions = newSessions, playing = False, active = newActive }
    in
    { model | sessions = newSessions_ }
        |> Misc.withCmd
        |> Misc.addCmd cmd
        |> Misc.addCmd
            (Cmd.batch
                [ model.settings |> Settings.encodeSettings |> Ports.localStorageHelper "settings"
                , newActive |> Sessions.encodeActive |> Ports.localStorageHelper "active"
                , Spotify.pauseCmd model.settings.spotify
                ]
            )



-- PORTS INTERFACE


type PortAction
    = RequestExport
    | Import String
    | Delete
    | TestAlarm String
    | RequestBrowserPermission Bool


encodePortAction : PortAction -> Encode.Value
encodePortAction actions =
    case actions of
        RequestExport ->
            Encode.object [ ( "type", Encode.string "requestExport" ) ]

        Import data ->
            Encode.object
                [ ( "type", Encode.string "import" )
                , ( "data", Encode.string data )
                ]

        Delete ->
            Encode.object [ ( "type", Encode.string "delete" ) ]

        TestAlarm sound ->
            Encode.object
                [ ( "type", Encode.string "testAlarm" )
                , ( "data", Encode.string sound )
                ]

        RequestBrowserPermission val ->
            Encode.object
                [ ( "type", Encode.string "browserPermission" )
                , ( "data", Encode.bool val )
                ]


toPort : PortAction -> Cmd msg
toPort =
    encodePortAction >> Ports.toSettings



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Ports.gotFromSettings GotBrowserNotificationPermission
        , Spotify.subscriptions |> Sub.map Spotify
        ]



-- CODECS


decodeBrowserNotificationPermission : Decode.Decoder { val : Bool, msg : String }
decodeBrowserNotificationPermission =
    Decode.map2
        (\val msg -> { val = val, msg = msg })
        (Decode.field "val" Decode.bool)
        (Decode.field "msg" Decode.string)
