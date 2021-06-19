port module Model exposing
    ( Model
    , buildIntervals
    , continuityFromString
    , continuityPairs
    , currentAddElapsed
    , currentElapsedPct
    , currentSecondsLeft
    , cycleBuild
    , cycleLog
    , cycleMaterialized
    , cycleStart
    , decodeCurrent
    , decodeLog
    , decodeNavLog
    , decodeSettings
    , decodeSpotify
    , default
    , encodeCurrent
    , encodeSettings
    , firstInterval
    , intervalIsActivity
    , intervalSeconds
    , intervalsTotalRun
    , mapSettings
    , themeFromString
    , themePairs
    )

import Browser.Navigation exposing (Key)
import Helpers
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import List.Extra as ListEx
import Msg exposing (Msg)
import Time exposing (Posix, Zone)
import Types
    exposing
        ( Continuity(..)
        , Current
        , Cycle
        , Interval(..)
        , Page(..)
        , Seconds
        , Settings
        , Spotify(..)
        , SpotifyPlaylist
        , Theme(..)
        )


port logCycle : E.Value -> Cmd msg


type alias Model =
    { zone : Zone
    , time : Posix
    , nav : Key
    , page : Page
    , uptime : Int
    , settings : Settings
    , current : Current
    , playing : Bool
    , intervals : List Interval
    }


defaultSettings : Settings
defaultSettings =
    Settings 4 (25 * 60) (5 * 60) (15 * 60) LightTheme NoCont Uninitialized


firstInterval : List Interval -> Interval
firstInterval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


buildIntervals : Settings -> Maybe Current -> ( List Interval, Current )
buildIntervals settings current =
    let
        intervals =
            settings.activity
                |> Activity
                |> List.repeat settings.rounds
                |> List.intersperse (Break settings.break)
                |> Helpers.flip (++) [ LongBreak settings.longBreak ]

        baseCurrent =
            Current 0 (cycleBuild (firstInterval intervals) Nothing) 0

        newCurrent =
            current
                |> Maybe.map
                    (\({ index, cycle } as curr) ->
                        case ListEx.getAt index intervals of
                            Just i ->
                                if i == cycle.interval then
                                    curr

                                else
                                    baseCurrent

                            Nothing ->
                                baseCurrent
                    )
                |> Maybe.withDefault baseCurrent
    in
    ( intervals, newCurrent )


cycleBuild : Interval -> Maybe Posix -> Cycle
cycleBuild interval start =
    let
        new =
            Cycle interval Nothing Nothing Nothing
    in
    start |> Maybe.map (\s -> { new | start = Just s }) |> Maybe.withDefault new


default : Key -> Model
default key =
    let
        ( intervals, current ) =
            buildIntervals defaultSettings Nothing
    in
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , nav = key
    , page = TimerPage
    , uptime = 0
    , settings = defaultSettings
    , current = current
    , playing = False
    , intervals = intervals
    }


cycleLog : Posix -> Current -> Cmd Msg
cycleLog now { cycle, elapsed } =
    if elapsed /= 0 then
        { cycle | end = Just now, seconds = Just elapsed }
            |> encodeCycle
            |> logCycle

    else
        Cmd.none


cycleStart : Posix -> Cycle -> Cycle
cycleStart now cycle =
    { cycle | start = Just now }


cycleMaterialized : Cycle -> Maybe { interval : Interval, start : Posix, end : Posix, seconds : Seconds }
cycleMaterialized { interval, start, end, seconds } =
    ( start, end, seconds )
        |> Helpers.maybeTrio
        |> Maybe.map
            (\( start_, end_, secs_ ) ->
                { interval = interval
                , start = start_
                , end = end_
                , seconds = secs_
                }
            )


intervalSeconds : Interval -> Seconds
intervalSeconds interval =
    case interval of
        Activity s ->
            s

        Break s ->
            s

        LongBreak s ->
            s


intervalsTotalRun : List Interval -> Seconds
intervalsTotalRun intervals =
    intervals |> List.foldl (\i t -> i |> intervalSeconds |> (+) t) 0


intervalIsActivity : Interval -> Bool
intervalIsActivity interval =
    case interval of
        Activity _ ->
            True

        _ ->
            False


currentSecondsLeft : Current -> Float
currentSecondsLeft { cycle, elapsed } =
    intervalSeconds cycle.interval - elapsed |> toFloat


currentAddElapsed : Int -> Current -> Current
currentAddElapsed i current =
    { current | elapsed = current.elapsed + i }


currentElapsedPct : Current -> Float
currentElapsedPct { cycle, elapsed } =
    toFloat elapsed * 100 / (toFloat <| intervalSeconds cycle.interval)


mapSettings : (Settings -> Settings) -> Model -> Model
mapSettings fn model =
    { model | settings = fn model.settings }


continuityPairs : List ( Continuity, String )
continuityPairs =
    [ ( NoCont, "No continuity" )
    , ( SimpleCont, "Simple continuity" )
    , ( FullCont, "Full continuity" )
    ]


continuityFromString : String -> Maybe Continuity
continuityFromString cont =
    continuityPairs
        |> ListEx.find (Tuple.second >> (==) cont)
        |> Maybe.map Tuple.first


themePairs : List ( Theme, String )
themePairs =
    [ ( LightTheme, "Light" )
    , ( DarkTheme, "Dark" )
    ]


themeFromString : String -> Maybe Theme
themeFromString theme =
    themePairs
        |> ListEx.find (Tuple.second >> (==) theme)
        |> Maybe.map Tuple.first


encodeInterval : Interval -> E.Value
encodeInterval interval =
    case interval of
        Activity s ->
            E.object
                [ ( "type", E.string "activity" )
                , ( "secs", E.int s )
                ]

        Break s ->
            E.object
                [ ( "type", E.string "break" )
                , ( "secs", E.int s )
                ]

        LongBreak s ->
            E.object
                [ ( "type", E.string "longbreak" )
                , ( "secs", E.int s )
                ]


encodeCycle : Cycle -> E.Value
encodeCycle { interval, start, end, seconds } =
    E.object
        [ ( "interval", encodeInterval interval )
        , ( "start", Helpers.encodeMaybe Helpers.encodePosix start )
        , ( "end", Helpers.encodeMaybe Helpers.encodePosix end )
        , ( "secs", Helpers.encodeMaybe E.int seconds )
        ]


encodeCurrent : Current -> E.Value
encodeCurrent { index, cycle, elapsed } =
    E.object
        [ ( "index", E.int index )
        , ( "cycle", encodeCycle cycle )
        , ( "elapsed", E.int elapsed )
        ]


encodeContinuity : Continuity -> E.Value
encodeContinuity cont =
    case cont of
        NoCont ->
            E.string "nocont"

        SimpleCont ->
            E.string "simplecont"

        FullCont ->
            E.string "fullcont"


encodeTheme : Theme -> E.Value
encodeTheme theme =
    case theme of
        LightTheme ->
            E.string "light"

        DarkTheme ->
            E.string "dark"


encodeSpotifyPlaylist : SpotifyPlaylist -> E.Value
encodeSpotifyPlaylist ( uri, title ) =
    E.object
        [ ( "uri", E.string uri )
        , ( "title", E.string title )
        ]


encodeSpotify : Spotify -> E.Value
encodeSpotify spotify =
    case spotify of
        NotConnected url ->
            E.object
                [ ( "type", E.string "notconnected" )
                , ( "url", E.string url )
                ]

        ConnectionError url ->
            E.object
                [ ( "type", E.string "connectionerror" )
                , ( "url", E.string url )
                ]

        Connected playlists playlist ->
            E.object
                [ ( "type", E.string "connected" )
                , ( "playlists", E.list encodeSpotifyPlaylist playlists )
                , ( "playlist", Helpers.encodeMaybe E.string playlist )
                ]

        Uninitialized ->
            E.object
                [ ( "type", E.string "uninitialized" ) ]


encodeSettings : Settings -> E.Value
encodeSettings { rounds, activity, break, longBreak, theme, continuity, spotify } =
    E.object
        [ ( "rounds", E.int rounds )
        , ( "activity", E.int activity )
        , ( "break", E.int break )
        , ( "longBreak", E.int longBreak )
        , ( "theme", encodeTheme theme )
        , ( "continuity", encodeContinuity continuity )
        , ( "spotify", encodeSpotify spotify )
        ]


decodeInterval : D.Decoder Interval
decodeInterval =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "activity" ->
                        D.map Activity <| D.field "secs" D.int

                    "break" ->
                        D.map Break <| D.field "secs" D.int

                    "longbreak" ->
                        D.map LongBreak <| D.field "secs" D.int

                    _ ->
                        D.fail <| "Can't decode interval of type: " ++ type_
            )


decodeCycle : D.Decoder Cycle
decodeCycle =
    D.succeed Cycle
        |> Pipeline.required "interval" decodeInterval
        |> Pipeline.required "start" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "end" (D.nullable Helpers.decodePosix)
        |> Pipeline.required "secs" (D.nullable D.int)


decodeLog : D.Decoder { ts : Int, daily : List Cycle, monthly : List Cycle }
decodeLog =
    D.succeed (\ts d m -> { ts = ts, daily = d, monthly = m })
        |> Pipeline.required "ts" D.int
        |> Pipeline.required "daily" (D.list decodeCycle)
        |> Pipeline.required "monthly" (D.list decodeCycle)


decodeNavLog : D.Decoder { ts : Int, log : List Cycle }
decodeNavLog =
    D.succeed (\ts l -> { ts = ts, log = l })
        |> Pipeline.required "ts" D.int
        |> Pipeline.required "log" (D.list decodeCycle)


decodeCurrent : D.Decoder Current
decodeCurrent =
    D.succeed Current
        |> Pipeline.required "index" D.int
        |> Pipeline.required "cycle" decodeCycle
        |> Pipeline.required "elapsed" D.int


decodeTheme : D.Decoder Theme
decodeTheme =
    D.string
        |> D.andThen
            (\theme ->
                case theme of
                    "light" ->
                        D.succeed LightTheme

                    "dark" ->
                        D.succeed DarkTheme

                    _ ->
                        D.fail <| "Could not find theme: " ++ theme
            )


decodeContinuity : D.Decoder Continuity
decodeContinuity =
    D.string
        |> D.andThen
            (\cont ->
                case cont of
                    "nocont" ->
                        D.succeed NoCont

                    "simplecont" ->
                        D.succeed SimpleCont

                    "fullcont" ->
                        D.succeed FullCont

                    _ ->
                        D.fail <| "Could not find continuity type: " ++ cont
            )


decodeSpotifyPlaylist : D.Decoder SpotifyPlaylist
decodeSpotifyPlaylist =
    D.map2 Tuple.pair
        (D.field "uri" D.string)
        (D.field "title" D.string)


decodeSpotify : D.Decoder Spotify
decodeSpotify =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "uninitialized" ->
                        D.succeed Uninitialized

                    "notconnected" ->
                        D.map NotConnected <| D.field "url" D.string

                    "connectionerror" ->
                        D.map ConnectionError <| D.field "url" D.string

                    "connected" ->
                        D.map2 Connected
                            (D.field "playlists" (D.list decodeSpotifyPlaylist))
                            (D.field "playlist" (D.nullable D.string))

                    _ ->
                        D.fail <| "Invalid spotify state of: " ++ type_
            )


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
