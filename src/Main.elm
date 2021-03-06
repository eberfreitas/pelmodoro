module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Events
import List.Extra as ListEx
import Platform exposing (Program)
import Task
import Time exposing (Posix, Zone)


type Msg
    = Tick Posix
    | AdjustTimeZone Zone
    | Pause
    | Play
    | Skip


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
            , Html.button [ Events.onClick Skip ] [ Html.text "Skip" ]
            ]
        ]


addElapsedSecond : Current -> Current
addElapsedSecond (Current p elapsed) =
    Current p (elapsed + 1)


firstInverval : List Interval -> Interval
firstInverval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


evalElapsedTime : Current -> Repeat -> List Interval -> ( Current, Bool )
evalElapsedTime ((Current ( idx, interval ) elapsed) as current) repeat intervals =
    if secondsLeft current == 0 then
        let
            firstInterval_ =
                intervals |> firstInverval
        in
        case ( intervals |> ListEx.getAt (idx + 1), repeat ) of
            ( Nothing, FullRepeat ) ->
                ( Current ( 0, firstInterval_ ) 0, True )

            ( Nothing, _ ) ->
                ( Current ( 0, firstInterval_ ) 0, False )

            ( Just nextInterval, NoRepeat ) ->
                ( Current ( idx + 1, nextInterval ) 0, False )

            ( Just nextInterval, _ ) ->
                ( Current ( idx + 1, nextInterval ) 0, True )

    else
        ( addElapsedSecond current, True )


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
                    ( newCurrent, newPlaying ) =
                        evalElapsedTime model.current model.repeat model.intervals
                in
                done { model | current = newCurrent, playing = newPlaying, time = posix }

            else
                done { model | time = posix }

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        Pause ->
            done { model | playing = False }

        Play ->
            done { model | playing = True }

        Skip ->
            let
                (Current ( idx, _ ) _) =
                    model.current

                ( nextIdx, nextInterval ) =
                    case ListEx.getAt (idx + 1) model.intervals of
                        Just next ->
                            ( idx + 1, next )

                        Nothing ->
                            ( 0, model.intervals |> firstInverval )

                newCurrent =
                    Current ( nextIdx, nextInterval ) 0
            in
            done { model | current = newCurrent, playing = False }


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
