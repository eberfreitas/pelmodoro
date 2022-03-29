module Settings exposing
    ( AlarmSound(..)
    , Flow
    , NotificationType(..)
    , Notifications
    , Settings
    , SpotifyState(..)
    , alarmSoundToEncodable
    , alarmSoundTypeAndStrings
    , decodeSettings
    , decodeSpotifyState
    , default
    , encodeNotifications
    , encodeSettings
    , flowTypeAndStrings
    , shouldKeepPlaying
    , toggleNotification
    )

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Misc
import Theme.Common
import Theme.Theme as Theme


type alias Settings =
    { rounds : Int
    , workDuration : Int
    , breakDuration : Int
    , longBreakDuration : Int
    , theme : Theme.Common.Theme
    , flow : Flow
    , spotify : SpotifyState
    , notifications : Notifications
    , alarmSound : AlarmSound
    }


type Flow
    = None
    | Simple
    | Loop


type SpotifyState
    = NotConnected String
    | ConnectionError String
    | Connected (List SpotifyPlaylist) (Maybe String)
    | Uninitialized


type alias SpotifyPlaylist =
    ( String, String )


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


default : Settings
default =
    Settings
        4
        (25 * 60)
        (5 * 60)
        (15 * 60)
        Theme.Common.Tomato
        None
        Uninitialized
        notificationsDefault
        WindChimes


notificationsDefault : Notifications
notificationsDefault =
    Notifications True True False


toggleNotification : NotificationType -> Notifications -> Notifications
toggleNotification type_ notification =
    case type_ of
        InApp ->
            { notification | inApp = not notification.inApp }

        AlarmSound ->
            { notification | alarmSound = not notification.alarmSound }

        Browser ->
            { notification | browser = not notification.browser }


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


encodeSpotifyState : SpotifyState -> Encode.Value
encodeSpotifyState state =
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


decodeSpotifyState : Decode.Decoder SpotifyState
decodeSpotifyState =
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
        , ( "spotify", encodeSpotifyState settings.spotify )
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
        |> Pipeline.required "spotify" decodeSpotifyState
        |> Pipeline.optional "notifications" decodeNotifications notificationsDefault
        |> Pipeline.optional "sound" decodeAlarmSound WindChimes
