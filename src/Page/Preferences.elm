module Page.Preferences exposing
    ( Model
    , Msg
    , new
    , subscriptions
    , update
    , view
    )

import Component.MiniTimer as MiniTimer
import Css
import Elements
import File
import File.Select as Select
import Flash
import Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Misc
import Page.Spotify as Spotify
import Ports
import Sessions
import Settings
import Task
import Theme.Theme as Theme
import Tuple.Trio as Trio



-- MODEL


type alias Model =
    { global : Global.Global }


new : Global.Global -> Model
new =
    Model



-- VIEW


view : Model -> Html.Html Msg
view { global } =
    let
        { settings } =
            global

        inMinutes seconds =
            seconds // 60
    in
    Html.div []
        [ MiniTimer.view global
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
update msg ({ global } as model) =
    let
        { settings } =
            global
    in
    case msg of
        UpdateRounds rounds ->
            model
                |> Global.mapSettings (\s -> { s | rounds = rounds })
                |> Misc.withCmd
                |> save

        UpdateWorkDuration secs ->
            model
                |> Global.mapSettings (\s -> { s | workDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateBreakDuration secs ->
            model
                |> Global.mapSettings (\s -> { s | breakDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateLongBreakDuration secs ->
            model
                |> Global.mapSettings (\s -> { s | longBreakDuration = secs * 60 })
                |> Misc.withCmd
                |> save

        UpdateFlow flow ->
            model
                |> Global.mapSettings
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
                |> Global.mapSettings
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
                |> Global.mapSettings
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
                            (model.global.settings.notifications.browser
                                |> not
                                |> RequestBrowserPermission
                                |> toPort
                            )

                _ ->
                    model
                        |> Global.mapSettings (\s -> { s | notifications = Settings.toggleNotification type_ s.notifications })
                        |> Misc.withCmd
                        |> save

        GotBrowserNotificationPermission raw ->
            case Decode.decodeValue decodeBrowserNotificationPermission raw of
                Ok res ->
                    let
                        notifications =
                            model.global.settings.notifications

                        newNotifications =
                            { notifications | browser = res.val }

                        flashMsg =
                            if res.msg /= "" then
                                Flash.new "Attention" res.msg

                            else
                                Nothing
                    in
                    model
                        |> Global.mapSettings (\s -> { s | notifications = newNotifications })
                        |> Global.modelSetFlash flashMsg
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
                |> Tuple.mapFirst (\state -> model |> Global.mapSettings (\s -> { s | spotify = state }))
                |> Misc.updateWith Spotify
                |> save



-- HELPERS


save : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
save ( { global }, cmd ) =
    let
        { sessions, settings } =
            global

        ( newSessions, newActive ) =
            Sessions.buildSessions settings (Just sessions.active)

        newSessions_ =
            { sessions | sessions = newSessions, playing = False, active = newActive }
    in
    { global | sessions = newSessions_ }
        |> Model
        |> Misc.withCmd
        |> Misc.addCmd cmd
        |> Misc.addCmd
            (Cmd.batch
                [ settings |> Settings.encodeSettings |> Ports.localStorageHelper "settings"
                , newActive |> Sessions.encodeActive |> Ports.localStorageHelper "active"
                , Spotify.pauseCmd settings.spotify
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
