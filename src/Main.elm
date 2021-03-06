module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Platform exposing (Program)
import Task
import Time exposing (Posix, Zone)


type Msg
    = Tick Posix
    | AdjustTimeZone Zone
    | Pause
    | Play


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type Repeat
    = NoRepeat
    | SimpleRepeat
    | FullRepeat


type alias Seconds =
    Int


type alias Settings =
    { activitiesCount : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    }


type alias Model =
    { zone : Zone
    , time : Posix
    , settings : Settings
    , current : Current
    , playing : Bool
    , repeat : Repeat
    , intervals : List Interval
    }


type Current
    = Current ( Int, Interval ) Int


defaultSettings : Settings
defaultSettings =
    Settings 4 (25 * 60) (5 * 60) (15 * 60)


defaultModel : Model
defaultModel =
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , settings = defaultSettings
    , current = Current ( 0, Activity (25 * 60) ) 0
    , playing = False
    , repeat = NoRepeat
    , intervals = buildIntervals defaultSettings
    }


flip : (b -> a -> c) -> a -> b -> c
flip fn a b =
    fn b a


buildIntervals : Settings -> List Interval
buildIntervals settings =
    settings.activity
        |> Activity
        |> List.repeat settings.activitiesCount
        |> List.intersperse (Break settings.break)
        |> flip (++) [ LongBreak settings.longBreak ]


init : () -> ( Model, Cmd Msg )
init () =
    ( defaultModel, Task.perform AdjustTimeZone Time.here )


secondsToDisplay : Seconds -> String
secondsToDisplay secs =
    let
        pad num =
            num |> String.fromInt |> String.padLeft 2 '0'
    in
    if secs < 60 then
        "00:" ++ pad secs

    else
        let
            min =
                (toFloat secs / 60) |> floor
        in
        pad min ++ ":" ++ pad (secs - (min * 60))


getSeconds : Interval -> Seconds
getSeconds interval =
    case interval of
        Activity secs ->
            secs

        Break secs ->
            secs

        LongBreak secs ->
            secs


secondsLeft : Current -> Seconds
secondsLeft (Current ( _, interval ) elapsed) =
    getSeconds interval - elapsed


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text <| secondsToDisplay (secondsLeft model.current) ]
        , Html.div []
            [ if model.playing then
                Html.button [ Events.onClick Pause ] [ Html.text "Pause" ]

              else
                Html.button [ Events.onClick Play ] [ Html.text "Start" ]
            ]
        ]


addElapsedSecond : Current -> Current
addElapsedSecond (Current p elapsed) =
    Current p (elapsed + 1)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        done m =
            ( m, Cmd.none )
    in
    case msg of
        Tick posix ->
            if model.playing == True then
                let
                    newCurrent =
                        model.current |> addElapsedSecond
                in
                done { model | current = newCurrent }

            else
                done model

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        Pause ->
            done { model | playing = False }

        Play ->
            done { model | playing = True }


subs : Model -> Sub Msg
subs model =
    Time.every 1000 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
