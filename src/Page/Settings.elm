module Page.Settings exposing (Msg, Settings)

import File exposing (File)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Misc
import Page.Spotify as Spotify exposing (Spotify)
import Theme.Common exposing (Theme(..))
import Theme.Theme as Theme


type alias Seconds =
    Int


type alias Notifications =
    { inApp : Bool
    , sound : Bool
    , browser : Bool
    }


type NotificationType
    = InApp
    | Sound
    | Browser


type AlarmSound
    = WindChimes
    | Bell
    | AlarmClock
    | Bong
    | RelaxingPercussion
    | BirdSong


type Flow
    = None
    | Simple
    | Loop


type alias Settings =
    { rounds : Int
    , workDuration : Seconds
    , breakDuration : Seconds
    , longBreakDuration : Seconds
    , theme : Theme
    , flow : Flow
    , spotify : Spotify
    , notifications : Notifications
    , alarmSound : AlarmSound
    }


type Msg
    = UpdateCount Int
    | UpdateWorkDuration Int
    | UpdateBreakDuration Int
    | UpdateLongBreakDuration Int
    | UpdateFlow String
    | UpdateTheme String
    | UpdateSpotifyPlaylist String
    | UpdateAlarmSound String
    | ToggleNotification NotificationType
    | GotBrowserNotificationPermission Value
    | ExportRequest
    | ImportRequest
    | ImportSelect File
    | ImportData String
    | TestSound AlarmSound
    | SpotifyMsg Spotify.Msg


default : Settings
default =
    Settings
        4
        (25 * 60)
        (5 * 60)
        (15 * 60)
        Tomato
        None
        Uninitialized
        notificationsDefault
        WindChimes


flowToString : Flow -> String
flowToString flow =
    case flow of
        None ->
            "none"

        Simple ->
            "simple"

        Loop ->
            "loop"


flowToDisplay : Flow -> String
flowToDisplay flow =
    case flow of
        None ->
            "No automatic flow"

        Simple ->
            "Simple flow"

        Loop ->
            "Non-stop loop"


flowList : List Flow
flowList =
    [ None
    , Simple
    , Loop
    ]


flowPairs : List ( Flow, String )
flowPairs =
    flowList |> Misc.toPairs flowToString


flowDisplayPairs : List ( Flow, String )
flowDisplayPairs =
    flowList |> Misc.toPairs flowToDisplay


flowFromString : String -> Maybe Flow
flowFromString =
    Misc.fromPairs flowPairs


flowFromDisplay : String -> Maybe Flow
flowFromDisplay =
    Misc.fromPairs flowDisplayPairs


notificationsDefault : Notifications
notificationsDefault =
    Notifications True True False


alarmSoundToString : AlarmSound -> String
alarmSoundToString alarmSound =
    case alarmSound of
        WindChimes ->
            "wind-chimes"

        Bell ->
            "bell"

        AlarmClock ->
            "alarm-clock"

        Bong ->
            "bong"

        RelaxingPercussion ->
            "relaxing-percussion"

        BirdSong ->
            "bird-song"


alarmSoundToDisplay : AlarmSound -> String
alarmSoundToDisplay alarmSound =
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


alarmSoundList : List AlarmSound
alarmSoundList =
    [ WindChimes
    , Bell
    , AlarmClock
    , Bong
    , RelaxingPercussion
    , BirdSong
    ]


alarmSoundPairs : List ( AlarmSound, String )
alarmSoundPairs =
    alarmSoundList |> Misc.toPairs alarmSoundToString


alarmSoundDisplayPairs : List ( AlarmSound, String )
alarmSoundDisplayPairs =
    alarmSoundList |> Misc.toPairs alarmSoundToDisplay


alarmSoundFromString : String -> Maybe AlarmSound
alarmSoundFromString =
    Misc.fromPairs alarmSoundPairs


alarmSoundFromDisplay : String -> Maybe AlarmSound
alarmSoundFromDisplay =
    Misc.fromPairs alarmSoundDisplayPairs



-- CODECS


encodeFlow : Flow -> Value
encodeFlow =
    flowToString >> E.string


decodeFlow : D.Decoder Flow
decodeFlow =
    D.string
        |> D.andThen
            (flowFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid flow")
            )


encodeNotifications : Notifications -> Value
encodeNotifications { inApp, sound, browser } =
    E.object
        [ ( "inapp", E.bool inApp )
        , ( "sound", E.bool sound )
        , ( "browser", E.bool browser )
        ]


decodeNotifications : D.Decoder Notifications
decodeNotifications =
    D.succeed Notifications
        |> Pipeline.required "inapp" D.bool
        |> Pipeline.required "sound" D.bool
        |> Pipeline.required "browser" D.bool


encodeAlarmSound : AlarmSound -> Value
encodeAlarmSound =
    alarmSoundToString >> E.string


decodeAlarmSound : D.Decoder AlarmSound
decodeAlarmSound =
    D.string
        |> D.andThen
            (alarmSoundFromString
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "Invalid alarm sound")
            )


encodeSettings : Settings -> Value
encodeSettings settings =
    E.object
        [ ( "rounds", E.int settings.rounds )
        , ( "workDuration", E.int settings.workDuration )
        , ( "breakDuration", E.int settings.breakDuration )
        , ( "longBreakDuration", E.int settings.longBreakDuration )
        , ( "theme", Theme.encodeTheme settings.theme )
        , ( "flow", encodeFlow settings.flow )
        , ( "spotify", encodeSpotify spotify )
        , ( "notifications", encodeNotifications notifications )
        , ( "sound", encodeSound sound )
        ]


decodeSettings : D.Decoder Settings
decodeSettings =
    D.succeed Settings
        |> Pipeline.required "rounds" D.int
        |> Pipeline.required "activity" D.int
        |> Pipeline.required "break" D.int
        |> Pipeline.required "longBreak" D.int
        |> Pipeline.required "theme" decodeTheme
        |> Pipeline.required "continuity" decodeContinuity
        |> Pipeline.required "spotify" decodeSpotify
        |> Pipeline.optional "notifications" decodeNotifications Tools.notificationsDefault
        |> Pipeline.optional "sound" decodeSound WindChimes
