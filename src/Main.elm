module Main exposing (main)

import Browser
import Html exposing (Html)
import Platform exposing (Program)


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type Repeat
    = NoRepeat
    | SimpleRepeat
    | FullRepeat


type State
    = Paused
    | Playing


type alias Model =
    { settings : Settings
    , current : Current
    , state : State
    , repeat : Repeat
    , intervals : List Interval
    }


type alias Settings =
    { activity : Int
    , activitiesCount : Int
    , break : Int
    , longBreak : Int
    }


type Current
    = Current ( Int, Interval ) Int


defaultSettings : Settings
defaultSettings =
    Settings 25 4 5 15


defaultModel : Model
defaultModel =
    { settings = defaultSettings
    , current = Current ( 0, Activity 25 ) 0
    , state = Paused
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


init : () -> ( Model, Cmd msg )
init () =
    ( defaultModel, Cmd.none )


view : Model -> Html msg
view _ =
    Html.text "Hello World!"


update : msg -> Model -> ( Model, Cmd msg )
update _ _ =
    ( defaultModel, Cmd.none )


subs : Model -> Sub msg
subs _ =
    Sub.none


main : Program () Model msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
