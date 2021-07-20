module Types exposing
    ( Continuity(..)
    , Current
    , Cycle
    , FlashMsg
    , Interval(..)
    , NotificationType(..)
    , Notifications
    , Page(..)
    , Seconds
    , Sentiment(..)
    , Settings
    , Spotify(..)
    , SpotifyPlaylist
    , StatState(..)
    , StatsDef
    )

import Date exposing (Date)
import Html.Styled exposing (Html)
import Themes.Types exposing (Theme)
import Time exposing (Posix)


type alias StatsDef =
    { date : Date
    , logs : List Cycle
    , showLogs : Bool
    }


type StatState
    = Loading
    | Loaded StatsDef


type Page
    = TimerPage
    | SettingsPage
    | StatsPage StatState
    | CreditsPage


type alias Seconds =
    Int


type alias SpotifyPlaylist =
    ( String, String )


type Spotify
    = NotConnected String
    | ConnectionError String
    | Connected (List SpotifyPlaylist) (Maybe String)
    | Uninitialized


type alias Notifications =
    { inApp : Bool
    , sound : Bool
    , browser : Bool
    }


type NotificationType
    = InApp
    | Sound
    | Browser


type alias Settings =
    { rounds : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    , theme : Theme
    , continuity : Continuity
    , spotify : Spotify
    , notifications : Notifications
    }


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type Sentiment
    = Positive
    | Neutral
    | Negative


type alias Cycle =
    { interval : Interval
    , start : Maybe Posix
    , end : Maybe Posix
    , seconds : Maybe Seconds
    , sentiment : Maybe Sentiment
    }


type alias Current =
    { index : Int
    , cycle : Cycle
    , elapsed : Seconds
    }


type Continuity
    = NoCont
    | SimpleCont
    | FullCont


type alias FlashMsg msg =
    { time : Int
    , title : String
    , msg : Html msg
    }
