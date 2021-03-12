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


port notify : () -> Cmd msg


type Msg
    = NoOp
    | Tick Posix
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


type ColorMode
    = Light
    | Dark


type alias Settings =
    { activitiesCount : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    , colorMode : ColorMode
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
    , log : Log
    }


type Feeling
    = Good
    | Neutral
    | Bad


type alias Cycle =
    { interval : Interval
    , start : Maybe Posix
    , end : Maybe Posix
    , feeling : Maybe Feeling
    , description : Maybe String
    , seconds : Maybe Seconds
    }


type alias Log =
    List Cycle


type Current
    = Current Int Cycle Seconds


defaultSettings : Settings
defaultSettings =
    Settings 4 (25 * 60) (5 * 60) (15 * 60) Light


newCycle : Interval -> Maybe Posix -> Cycle
newCycle interval start =
    let
        new =
            Cycle interval Nothing Nothing Nothing Nothing Nothing
    in
    start |> Maybe.map (\s -> { new | start = Just s }) |> Maybe.withDefault new


logCycle : Posix -> Current -> Log -> Log
logCycle now (Current _ cycle elapsed) log =
    { cycle | end = Just now, seconds = Just elapsed }
        |> List.singleton
        |> (++) log


startCycle : Posix -> Cycle -> Cycle
startCycle now cycle =
    { cycle | start = Just now }


defaultModel : Model
defaultModel =
    let
        intervals =
            buildIntervals defaultSettings

        firstInterval_ =
            firstInverval intervals

        current =
            Current 0 (newCycle firstInterval_ Nothing) 0
    in
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , uptime = 0
    , settings = defaultSettings
    , current = current
    , playing = False
    , repeat = NoRepeat
    , intervals = intervals
    , log = []
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


intervalToColor : ColorMode -> Interval -> String
intervalToColor mode interval =
    case ( mode, interval ) of
        ( Light, Activity _ ) ->
            "tomato"

        ( Light, Break _ ) ->
            "#2D5BDE"

        ( Light, LongBreak _ ) ->
            "#2DBCE0"

        _ ->
            "TBD"


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


intervalToString : Interval -> String
intervalToString interval =
    case interval of
        Activity _ ->
            "activity"

        Break _ ->
            "break"

        LongBreak _ ->
            "longbreak"


secondsLeft : Current -> Seconds
secondsLeft (Current _ { interval } elapsed) =
    getSeconds interval - elapsed


renderIntervals : ColorMode -> Current -> List Interval -> Html Msg
renderIntervals colorMode ((Current index _ _) as current) intervals =
    intervals
        |> List.indexedMap
            (\idx interval ->
                if index > idx then
                    Html.div
                        [ HtmlAttrs.class "bullet done"
                        , HtmlAttrs.style "background-color" (intervalToColor colorMode interval)
                        ]
                        []
                        |> List.singleton
                        |> Html.li [ HtmlAttrs.class (intervalToString interval) ]

                else if index < idx then
                    Html.div
                        [ HtmlAttrs.class "bullet undone"
                        , HtmlAttrs.style "background-color" (intervalToColor colorMode interval)
                        ]
                        []
                        |> List.singleton
                        |> Html.li [ HtmlAttrs.class (intervalToString interval) ]

                else
                    [ Html.div
                        [ HtmlAttrs.class "bullet ongoing"
                        , HtmlAttrs.style "background-color" (intervalToColor colorMode interval)
                        ]
                        []
                    , Html.div
                        [ HtmlAttrs.class "ongoing-indicator"
                        , HtmlAttrs.style "width" (elapsedPercentage current |> String.fromInt |> flip (++) "%")
                        , HtmlAttrs.style "height" (elapsedPercentage current |> String.fromInt |> flip (++) "%")
                        , HtmlAttrs.style "top" (elapsedPercentage current |> (-) 100 |> toFloat |> flip (/) 2 |> String.fromFloat |> flip (++) "%")
                        , HtmlAttrs.style "left" (elapsedPercentage current |> (-) 100 |> toFloat |> flip (/) 2 |> String.fromFloat |> flip (++) "%")
                        , HtmlAttrs.style "background-color" (intervalToColor colorMode interval)
                        ]
                        []
                    ]
                        |> Html.li [ HtmlAttrs.class (intervalToString interval) ]
            )
        |> Html.ul [ HtmlAttrs.class "intervals" ]


renderControls : Bool -> Repeat -> Html Msg
renderControls playing repeat =
    Html.div [ HtmlAttrs.class "controls" ]
        [ if playing then
            Html.button [ Events.onClick Pause ] [ Html.i [ HtmlAttrs.class "fas fa-pause " ] [] ]

          else
            Html.button [ Events.onClick Play ] [ Html.i [ HtmlAttrs.class "fas fa-play " ] [] ]
        , Html.button [ Events.onClick Skip ] [ Html.i [ HtmlAttrs.class "fas fa-forward " ] [] ]
        , case repeat of
            NoRepeat ->
                Html.button [ Events.onClick (SetRepeat SimpleRepeat), HtmlAttrs.class "no-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]

            SimpleRepeat ->
                Html.button [ Events.onClick (SetRepeat FullRepeat), HtmlAttrs.class "simple-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]

            FullRepeat ->
                Html.button [ Events.onClick (SetRepeat NoRepeat), HtmlAttrs.class "full-repeat" ] [ Html.i [ HtmlAttrs.class "fas fa-redo-alt" ] [] ]
        ]


renderTimer : Model -> Html Msg
renderTimer model =
    let
        timerOpacity =
            if model.playing == True then
                "100"

            else if (model.uptime |> modBy 2) == 0 then
                "100"

            else
                "0"

        (Current _ { interval } _) =
            model.current

        timerRadius =
            110

        timerCircunference =
            2 * pi * timerRadius |> truncate

        timerSection =
            model.current |> elapsedPercentage |> circunferenceSection timerCircunference
    in
    Svg.svg [ SvgAttrs.width "240", SvgAttrs.height "240", SvgAttrs.viewBox "0 0 240 240", SvgAttrs.class "timer" ]
        [ Svg.circle
            [ SvgAttrs.cx "50%"
            , SvgAttrs.cy "50%"
            , SvgAttrs.r (String.fromInt timerRadius)
            , SvgAttrs.fill "none"
            , SvgAttrs.stroke <| intervalToColor model.settings.colorMode interval
            , SvgAttrs.strokeWidth "20"
            , SvgAttrs.strokeOpacity "0.25"
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx "120"
            , SvgAttrs.cy "120"
            , SvgAttrs.r (String.fromInt timerRadius)
            , SvgAttrs.fill "none"
            , SvgAttrs.stroke <| intervalToColor model.settings.colorMode interval
            , SvgAttrs.strokeWidth "20"
            , SvgAttrs.transform "rotate(-90, 120, 120) scale(1, -1) translate(0, -240)"
            , SvgAttrs.strokeDasharray (String.fromInt timerCircunference)
            , SvgAttrs.strokeDashoffset (String.fromInt timerSection)
            ]
            []
        , Svg.text_
            [ SvgAttrs.x "50%"
            , SvgAttrs.y "55%"
            , SvgAttrs.textAnchor "middle"
            , SvgAttrs.fill <| intervalToColor model.settings.colorMode interval
            , SvgAttrs.fontFamily "Montserrat"
            , SvgAttrs.fontSize "36px"
            , SvgAttrs.opacity timerOpacity
            ]
            [ Svg.text <| secondsToDisplay (secondsLeft model.current) ]
        ]


view : Model -> Html Msg
view model =
    Html.div [ HtmlAttrs.class "container" ]
        [ Html.div [ HtmlAttrs.class "main" ]
            [ renderTimer model
            , renderIntervals model.settings.colorMode model.current model.intervals
            , renderControls model.playing model.repeat
            ]
        ]


addElapsedSecond : Current -> Current
addElapsedSecond (Current i p elapsed) =
    Current i p (elapsed + 1)


firstInverval : List Interval -> Interval
firstInverval =
    List.head >> Maybe.withDefault (Activity (25 * 60))


evalElapsedTime : Posix -> Current -> Repeat -> List Interval -> ( Current, Bool, Cmd msg )
evalElapsedTime now ((Current index { interval } elapsed) as current) repeat intervals =
    if secondsLeft current == 0 then
        let
            firstInterval_ =
                intervals |> firstInverval

            ( current_, playing ) =
                case ( intervals |> ListEx.getAt (index + 1), repeat ) of
                    ( Nothing, FullRepeat ) ->
                        ( Current 0 (newCycle firstInterval_ (Just now)) 0, True )

                    ( Nothing, _ ) ->
                        ( Current 0 (newCycle firstInterval_ Nothing) 0, False )

                    ( Just nextInterval, NoRepeat ) ->
                        ( Current (index + 1) (newCycle nextInterval Nothing) 0, False )

                    ( Just nextInterval, _ ) ->
                        ( Current (index + 1) (newCycle nextInterval (Just now)) 0, True )
        in
        ( current_, playing, notify () )

    else
        ( addElapsedSecond current, True, Cmd.none )


elapsedPercentage : Current -> Int
elapsedPercentage (Current _ { interval } elapsed) =
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
        NoOp ->
            done model

        Tick posix ->
            if model.playing == True then
                let
                    ( newCurrent, newPlaying, cmd ) =
                        evalElapsedTime model.time model.current model.repeat model.intervals

                    newLog =
                        if cmd /= Cmd.none then
                            model.log |> logCycle model.time model.current

                        else
                            model.log
                in
                ( { model | current = newCurrent, playing = newPlaying, time = posix, uptime = model.uptime + 1, log = newLog }, cmd )

            else
                done { model | time = posix, uptime = model.uptime + 1 }

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        Pause ->
            done { model | playing = False }

        Play ->
            let
                (Current index cycle _) =
                    model.current
            in
            done { model | playing = True, current = Current index (startCycle model.time cycle) 0 }

        Skip ->
            let
                (Current index _ _) =
                    model.current

                ( nextIndex, nextInterval ) =
                    case ListEx.getAt (index + 1) model.intervals of
                        Just next ->
                            ( index + 1, next )

                        Nothing ->
                            ( 0, model.intervals |> firstInverval )

                newCurrent =
                    Current nextIndex (newCycle nextInterval Nothing) 0

                newLog =
                    model.log |> logCycle model.time model.current
            in
            done { model | current = newCurrent, playing = False, log = newLog }

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
