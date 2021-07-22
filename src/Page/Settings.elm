module Page.Settings exposing
    ( Flow
    , Msg
    , Notifications
    , Settings
    , alarmSoundToString
    , decodeSettings
    , default
    , encodeNotifications
    , shouldKeepPlaying
    , subscriptions
    , update
    )

import File
import File.Select as Select
import Html.Styled as Html
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Misc
import Page.Flash as Flash
import Page.Spotify as Spotify
import Ports
import Task
import Theme.Common
import Theme.Theme as Theme



-- MODEL


type alias Model a msg =
    { a | settings : Settings, flash : Maybe (Flash.FlashMsg msg) }


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


update : Msg -> Model a msg -> ( Model a msg, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        UpdateRounds rounds ->
            model
                |> mapSettings (\s -> { s | rounds = rounds })
                |> Misc.withCmd
                |> save

        UpdateWorkDuration secs ->
            model
                |> mapSettings (\s -> { s | workDuration = secs })
                |> Misc.withCmd
                |> save

        UpdateBreakDuration secs ->
            model
                |> mapSettings (\s -> { s | breakDuration = secs })
                |> Misc.withCmd
                |> save

        UpdateLongBreakDuration secs ->
            model
                |> mapSettings (\s -> { s | longBreakDuration = secs })
                |> Misc.withCmd
                |> save

        UpdateFlow flow ->
            model
                |> mapSettings
                    (\s ->
                        flow
                            |> flowFromString
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
                            |> Theme.themeFromString
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
                            |> alarmSoundFromString
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
                |> Misc.addCmd (alarmSound |> alarmSoundToString |> TestAlarm |> toPort)

        Spotify subMsg ->
            Spotify.update subMsg settings.spotify
                |> Tuple.mapFirst (\state -> model |> mapSettings (\s -> { s | spotify = state }))
                |> Misc.updateWith Spotify
                |> save



-- HELPERS


mapSettings : (Settings -> Settings) -> Model a msg -> Model a msg
mapSettings fn model =
    { model | settings = fn model.settings }


save : ( Model a msg, Cmd Msg ) -> ( Model a msg, Cmd Msg )
save (( { settings }, _ ) as pair) =
    settings
        |> encodeSettings
        |> Ports.localStorageHelper "settings"
        |> Misc.flip Misc.addCmd pair


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


flowToString : Flow -> String
flowToString flow =
    case flow of
        None ->
            "No automatic flow"

        Simple ->
            "Simple flow"

        Loop ->
            "Non-stop flow"


flowPairs : List ( Flow, String )
flowPairs =
    [ None
    , Simple
    , Loop
    ]
        |> Misc.toPairs flowToString


flowFromString : String -> Maybe Flow
flowFromString =
    Misc.fromPairs flowPairs


notificationsDefault : Notifications
notificationsDefault =
    Notifications True True False


alarmSoundToString : AlarmSound -> String
alarmSoundToString alarmSound =
    case alarmSound of
        WindChimes ->
            "Wind Chimes"

        Bell ->
            "Bell"

        AlarmClock ->
            "Alarm Clock"

        Bong ->
            "Bong"

        RelaxingPercussion ->
            "Relaxing Percussion"

        BirdSong ->
            "Bird Song"


alarmSoundPairs : List ( AlarmSound, String )
alarmSoundPairs =
    [ WindChimes
    , Bell
    , AlarmClock
    , Bong
    , RelaxingPercussion
    , BirdSong
    ]
        |> Misc.toPairs alarmSoundToString


alarmSoundFromString : String -> Maybe AlarmSound
alarmSoundFromString =
    Misc.fromPairs alarmSoundPairs



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
    flowToString >> Encode.string


decodeFlow : Decode.Decoder Flow
decodeFlow =
    Decode.string
        |> Decode.andThen
            (flowFromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid flow")
            )


encodeNotifications : Notifications -> Encode.Value
encodeNotifications { inApp, alarmSound, browser } =
    Encode.object
        [ ( "inApp", Encode.bool inApp )
        , ( "alarmSound", Encode.bool alarmSound )
        , ( "browser", Encode.bool browser )
        ]


decodeNotifications : Decode.Decoder Notifications
decodeNotifications =
    Decode.succeed Notifications
        |> Pipeline.required "inApp" Decode.bool
        |> Pipeline.required "alarmSound" Decode.bool
        |> Pipeline.required "browser" Decode.bool


encodeAlarmSound : AlarmSound -> Encode.Value
encodeAlarmSound =
    alarmSoundToString >> Encode.string


decodeAlarmSound : Decode.Decoder AlarmSound
decodeAlarmSound =
    Decode.string
        |> Decode.andThen
            (alarmSoundFromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Invalid alarm sound")
            )


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "rounds", Encode.int settings.rounds )
        , ( "workDuration", Encode.int settings.workDuration )
        , ( "breakDuration", Encode.int settings.breakDuration )
        , ( "longBreakDuration", Encode.int settings.longBreakDuration )
        , ( "theme", Theme.encodeTheme settings.theme )
        , ( "flow", encodeFlow settings.flow )
        , ( "spotify", Spotify.encodeState settings.spotify )
        , ( "notifications", encodeNotifications settings.notifications )
        , ( "alarmSound", encodeAlarmSound settings.alarmSound )
        ]


decodeSettings : Decode.Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> Pipeline.required "rounds" Decode.int
        |> Pipeline.required "workDuration" Decode.int
        |> Pipeline.required "breakDuration" Decode.int
        |> Pipeline.required "longBreakDuration" Decode.int
        |> Pipeline.required "theme" Theme.decodeTheme
        |> Pipeline.required "flow" decodeFlow
        |> Pipeline.required "spotify" Spotify.decodeState
        |> Pipeline.optional "notifications" decodeNotifications notificationsDefault
        |> Pipeline.optional "alarmSound" decodeAlarmSound WindChimes


decodeBrowserNotificationPermission : Decode.Decoder { val : Bool, msg : String }
decodeBrowserNotificationPermission =
    Decode.map2
        (\val msg -> { val = val, msg = msg })
        (Decode.field "val" Decode.bool)
        (Decode.field "msg" Decode.string)
