module Page.Settings exposing
    ( Flow
    , Msg
    , Notifications
    , Settings
    , alarmSoundToEncodable
    , decodeSettings
    , default
    , encodeNotifications
    , shouldKeepPlaying
    , subscriptions
    , update
    , view
    )

import Css
import Elements
import File
import File.Select as Select
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Misc
import Page.Flash as Flash
import Page.MiniTimer as MiniTimer
import Page.Spotify as Spotify
import Ports
import Session
import Task
import Theme.Common
import Theme.Theme as Theme
import Tuple.Trio as Trio



-- MODEL


type alias Model a =
    { a
        | settings : Settings
        , flash : Maybe (Flash.FlashMsg Flash.Msg)
        , active : Session.Active
        , sessions : List Session.SessionDef
        , playing : Bool
    }


type alias Settings =
    { rounds : Int
    , workDuration : Seconds
    , breakDuration : Seconds
    , longBreakDuration : Seconds
    , theme : Theme.Common.Theme
    , flow : Flow
    , spotify : Spotify.State
    , notifications : Notifications
    , alarmSound : AlarmSound
    }


type alias Seconds =
    Int


type Flow
    = None
    | Simple
    | Loop


type alias Notifications =
    { inApp : Bool
    , alarmSound : Bool
    , browser : Bool
    }


type NotificationType
    = InApp
    | AlarmSound
    | Browser


type AlarmSound
    = WindChimes
    | Bell
    | AlarmClock
    | Bong
    | RelaxingPercussion
    | BirdSong



-- VIEW


view : Model a -> Html.Html Msg
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
            , Elements.inputContainer settings.theme "Rounds" <|
                Elements.numberInput settings.theme 1 8 UpdateRounds settings.rounds
            , Elements.inputContainer settings.theme "Session duration" <|
                Elements.numberInput settings.theme 1 60 UpdateWorkDuration <|
                    inMinutes settings.workDuration
            , Elements.inputContainer settings.theme "Break duration" <|
                Elements.numberInput settings.theme 1 60 UpdateBreakDuration <|
                    inMinutes settings.breakDuration
            , Elements.inputContainer settings.theme "Long break duration" <|
                Elements.numberInput settings.theme 1 60 UpdateLongBreakDuration <|
                    inMinutes settings.longBreakDuration
            , Elements.inputContainer settings.theme "Rounds flow" <|
                Elements.selectInput settings.theme
                    (Trio.first >> (==) settings.flow)
                    UpdateFlow
                    flowTypeAndStrings
            , Elements.inputContainer settings.theme
                "Notifications"
                ([ ( settings.notifications.inApp, InApp, "In app messages" )
                 , ( settings.notifications.alarmSound, AlarmSound, "Play sounds" )
                 , ( settings.notifications.browser, Browser, "Browser notification" )
                 ]
                    |> List.map (\( v, t, l ) -> Elements.checkbox settings.theme v (ToggleNotification t) l)
                    |> Html.div []
                )
            , if settings.notifications.alarmSound then
                Elements.inputContainer settings.theme "Alarm sound" <|
                    Html.div []
                        [ Elements.selectInput settings.theme
                            (Trio.first >> (==) settings.alarmSound)
                            UpdateAlarmSound
                            alarmSoundTypeAndStrings
                        , Elements.largeButton settings.theme
                            (TestAlarmSound settings.alarmSound)
                            [ Elements.styledIcon [ Css.verticalAlign Css.middle ] "play_arrow" ]
                        ]

              else
                Html.text ""
            , Elements.inputContainer settings.theme "Color theme" <|
                Elements.selectInput settings.theme
                    (Trio.first >> (==) settings.theme)
                    UpdateTheme
                    Theme.themeTypeAndStrings
            , Spotify.view settings.theme settings.spotify |> Html.map Spotify
            , Elements.inputContainer settings.theme "Import / Export" <|
                Html.div []
                    [ Elements.largeButton settings.theme ExportRequest [ Html.text "Export" ]
                    , Elements.largeButton settings.theme ImportRequest [ Html.text "Import" ]
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
    | ToggleNotification NotificationType
    | GotBrowserNotificationPermission Decode.Value
    | ExportRequest
    | ImportRequest
    | ImportSelect File.File
    | ClearLogs
    | ImportData String
    | TestAlarmSound AlarmSound
    | Spotify Spotify.Msg


update : Msg -> Model a -> ( Model a, Cmd Msg )
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
                            |> Misc.encodableToType flowTypeAndStrings
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
                            |> Misc.encodableToType alarmSoundTypeAndStrings
                            |> Maybe.map (\a -> { s | alarmSound = a })
                            |> Maybe.withDefault s
                    )
                |> Misc.withCmd
                |> save

        ToggleNotification type_ ->
            case type_ of
                Browser ->
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
                        |> mapSettings (\s -> { s | notifications = toggleNotification type_ s.notifications })
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
                        |> Misc.typeToEncodable alarmSoundTypeAndStrings
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


mapSettings : (Settings -> Settings) -> Model a -> Model a
mapSettings fn model =
    { model | settings = fn model.settings }


save : ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
save ( model, cmd ) =
    let
        ( newSessions, newActive ) =
            Session.buildSessions model.settings (Just model.active)
    in
    { model | playing = False, sessions = newSessions, active = newActive }
        |> Misc.withCmd
        |> Misc.addCmd cmd
        |> Misc.addCmd
            (Cmd.batch
                [ model.settings |> encodeSettings |> Ports.localStorageHelper "settings"
                , newActive |> Session.encodeActive |> Ports.localStorageHelper "active"
                ]
            )


toggleNotification : NotificationType -> Notifications -> Notifications
toggleNotification type_ notification =
    case type_ of
        InApp ->
            { notification | inApp = not notification.inApp }

        AlarmSound ->
            { notification | alarmSound = not notification.alarmSound }

        Browser ->
            { notification | browser = not notification.browser }


default : Settings
default =
    Settings
        4
        (25 * 60)
        (5 * 60)
        (15 * 60)
        Theme.Common.Tomato
        None
        Spotify.default
        notificationsDefault
        WindChimes


shouldKeepPlaying : Int -> Flow -> Bool
shouldKeepPlaying index flow =
    case ( index, flow ) of
        ( _, None ) ->
            False

        ( 0, Simple ) ->
            False

        ( 0, Loop ) ->
            True

        _ ->
            True


flowTypeAndStrings : Misc.TypeAndStrings Flow
flowTypeAndStrings =
    [ ( None, "nocont", "No automatic flow" )
    , ( Simple, "simplecont", "Simple flow" )
    , ( Loop, "fullcont", "Non-stop flow" )
    ]


notificationsDefault : Notifications
notificationsDefault =
    Notifications True True False


alarmSoundTypeAndStrings : Misc.TypeAndStrings AlarmSound
alarmSoundTypeAndStrings =
    [ ( WindChimes, "wind-chimes", "Wind Chimes" )
    , ( Bell, "bell", "Bell" )
    , ( AlarmClock, "alarm-clock", "Alarm Clock" )
    , ( Bong, "bong", "Bong" )
    , ( RelaxingPercussion, "relaxing-percussion", "Relaxing Percussion" )
    , ( BirdSong, "bird-song", "Bird Song" )
    ]


alarmSoundToEncodable : AlarmSound -> String
alarmSoundToEncodable =
    Misc.typeToEncodable alarmSoundTypeAndStrings >> Maybe.withDefault ""



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


encodeFlow : Flow -> Encode.Value
encodeFlow =
    Misc.typeToEncodable flowTypeAndStrings >> Maybe.withDefault "" >> Encode.string


decodeFlow : Decode.Decoder Flow
decodeFlow =
    Decode.string
        |> Decode.andThen
            (Misc.encodableToType flowTypeAndStrings
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid flow")
            )


encodeNotifications : Notifications -> Encode.Value
encodeNotifications { inApp, alarmSound, browser } =
    Encode.object
        [ ( "inapp", Encode.bool inApp )
        , ( "sound", Encode.bool alarmSound )
        , ( "browser", Encode.bool browser )
        ]


decodeNotifications : Decode.Decoder Notifications
decodeNotifications =
    Decode.succeed Notifications
        |> Pipeline.required "inapp" Decode.bool
        |> Pipeline.required "sound" Decode.bool
        |> Pipeline.required "browser" Decode.bool


encodeAlarmSound : AlarmSound -> Encode.Value
encodeAlarmSound =
    Misc.typeToEncodable alarmSoundTypeAndStrings >> Maybe.withDefault "" >> Encode.string


decodeAlarmSound : Decode.Decoder AlarmSound
decodeAlarmSound =
    Decode.string
        |> Decode.andThen
            (Misc.encodableToType alarmSoundTypeAndStrings
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid alarm sound")
            )


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "rounds", Encode.int settings.rounds )
        , ( "activity", Encode.int settings.workDuration )
        , ( "break", Encode.int settings.breakDuration )
        , ( "longBreak", Encode.int settings.longBreakDuration )
        , ( "theme", Theme.encodeTheme settings.theme )
        , ( "continuity", encodeFlow settings.flow )
        , ( "spotify", Spotify.encodeState settings.spotify )
        , ( "notifications", encodeNotifications settings.notifications )
        , ( "sound", encodeAlarmSound settings.alarmSound )
        ]


decodeSettings : Decode.Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> Pipeline.required "rounds" Decode.int
        |> Pipeline.required "activity" Decode.int
        |> Pipeline.required "break" Decode.int
        |> Pipeline.required "longBreak" Decode.int
        |> Pipeline.required "theme" Theme.decodeTheme
        |> Pipeline.required "continuity" decodeFlow
        |> Pipeline.required "spotify" Spotify.decodeState
        |> Pipeline.optional "notifications" decodeNotifications notificationsDefault
        |> Pipeline.optional "sound" decodeAlarmSound WindChimes


decodeBrowserNotificationPermission : Decode.Decoder { val : Bool, msg : String }
decodeBrowserNotificationPermission =
    Decode.map2
        (\val msg -> { val = val, msg = msg })
        (Decode.field "val" Decode.bool)
        (Decode.field "msg" Decode.string)
