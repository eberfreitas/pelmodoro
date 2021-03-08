port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events as Events
import List.Extra as ListEx
import Platform exposing (Program)
import Svg
import Svg.Attributes as SvgAttrs
import Task
import Time exposing (Posix, Zone)


port playSound : () -> Cmd msg


type Msg
    = Tick Posix
    | AdjustTimeZone Zone
    | Pause
    | Play
    | Skip
    | SetRepeat Repeat


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
    , uptime : Int
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
    , uptime = 0
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
        "0:" ++ pad secs

    else
        let
            min =
                (toFloat secs / 60) |> floor
        in
        String.fromInt min ++ ":" ++ pad (secs - (min * 60))


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
    let
        timerOpacity =
            if model.playing == True then
                "100"

            else if (model.uptime |> modBy 2) == 0 then
                "100"

            else
                "0"

        timerColor =
            case model.current of
                Current ( _, Activity _ ) _ ->
                    "tomato"

                Current ( _, Break _ ) _ ->
                    "#2D5BDE"

                Current ( _, LongBreak _ ) _ ->
                    "#2DBCE0"

        timerRadius =
            110

        timerCircunference =
            2 * pi * timerRadius |> truncate

        timerSection =
            model.current |> elapsedPercentage |> circunferenceSection timerCircunference
    in
    Html.div [ HtmlAttrs.class "container" ]
        [ Html.div [ HtmlAttrs.class "main" ]
            [ Svg.svg [ SvgAttrs.width "240", SvgAttrs.height "240", SvgAttrs.viewBox "0 0 240 240" ]
                -- Activity circle
                [ Svg.circle
                    [ SvgAttrs.cx "50%"
                    , SvgAttrs.cy "50%"
                    , SvgAttrs.r (String.fromInt timerRadius)
                    , SvgAttrs.fill "none"
                    , SvgAttrs.stroke timerColor
                    , SvgAttrs.strokeWidth "20"
                    , SvgAttrs.strokeOpacity "0.25"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.cx "120"
                    , SvgAttrs.cy "120"
                    , SvgAttrs.r (String.fromInt timerRadius)
                    , SvgAttrs.fill "none"
                    , SvgAttrs.stroke timerColor
                    , SvgAttrs.strokeWidth "20"
                    , SvgAttrs.transform "rotate(-90, 120, 120) scale(1, -1) translate(0, -240)"
                    , SvgAttrs.strokeDasharray (String.fromInt timerCircunference)
                    , SvgAttrs.strokeDashoffset (String.fromInt timerSection)
                    ]
                    []

                -- Timer
                , Svg.text_
                    [ SvgAttrs.x "50%"
                    , SvgAttrs.y "55%"
                    , SvgAttrs.textAnchor "middle"
                    , SvgAttrs.fill timerColor
                    , SvgAttrs.fontFamily "Montserrat"
                    , SvgAttrs.fontSize "36px"
                    , SvgAttrs.opacity timerOpacity
                    ]
                    [ Svg.text <| secondsToDisplay (secondsLeft model.current) ]
                ]
            , Html.div [ HtmlAttrs.class "controls" ]
                [ if model.playing then
                    Html.button [ Events.onClick Pause ] [ Html.i [ HtmlAttrs.class "fas fa-pause " ] [] ]

                  else
                    Html.button [ Events.onClick Play ] [ Html.i [ HtmlAttrs.class "fas fa-play " ] [] ]
                , Html.button [ Events.onClick Skip ] [ Html.i [ HtmlAttrs.class "fas fa-forward " ] [] ]
                , case model.repeat of
                    NoRepeat ->
                        Html.button [ Events.onClick (SetRepeat SimpleRepeat), HtmlAttrs.class "no-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]

                    SimpleRepeat ->
                        Html.button [ Events.onClick (SetRepeat FullRepeat), HtmlAttrs.class "simple-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]

                    FullRepeat ->
                        Html.button [ Events.onClick (SetRepeat NoRepeat), HtmlAttrs.class "full-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]
                ]
            ]
        ]


addElapsedSecond : Current -> Current
addElapsedSecond (Current p elapsed) =
    Current p (elapsed + 1)


firstInverval : List Interval -> Interval
firstInverval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


evalElapsedTime : Current -> Repeat -> List Interval -> ( Current, Bool, Cmd msg )
evalElapsedTime ((Current ( idx, interval ) elapsed) as current) repeat intervals =
    if secondsLeft current == 0 then
        let
            firstInterval_ =
                intervals |> firstInverval

            ( current_, playing ) =
                case ( intervals |> ListEx.getAt (idx + 1), repeat ) of
                    ( Nothing, FullRepeat ) ->
                        ( Current ( 0, firstInterval_ ) 0, True )

                    ( Nothing, _ ) ->
                        ( Current ( 0, firstInterval_ ) 0, False )

                    ( Just nextInterval, NoRepeat ) ->
                        ( Current ( idx + 1, nextInterval ) 0, False )

                    ( Just nextInterval, _ ) ->
                        ( Current ( idx + 1, nextInterval ) 0, True )
        in
        ( current_, playing, playSound () )

    else
        ( addElapsedSecond current, True, Cmd.none )


elapsedPercentage : Current -> Int
elapsedPercentage (Current ( _, interval ) elapsed) =
    (toFloat elapsed * 100 / (toFloat <| getSeconds interval)) |> truncate


circunferenceSection : Int -> Int -> Int
circunferenceSection circunference percentage =
    toFloat circunference * toFloat percentage / 100.0 |> truncate


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
                    ( newCurrent, newPlaying, cmd ) =
                        evalElapsedTime model.current model.repeat model.intervals
                in
                ( { model | current = newCurrent, playing = newPlaying, time = posix, uptime = model.uptime + 1 }, cmd )

            else
                done { model | time = posix, uptime = model.uptime + 1 }

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

        SetRepeat repeat ->
            done { model | repeat = repeat }


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
