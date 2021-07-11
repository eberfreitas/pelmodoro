module Types exposing
    ( Continuity(..)
    , Current
    , Cycle
    , Interval(..)
    , Page(..)
    , Seconds
    , Settings
    , Spotify(..)
    , SpotifyPlaylist
    , StatState(..)
    , StatsDef
    )

import Date exposing (Date)
import Themes.Types exposing (Theme)
import Time exposing (Posix)


type alias StatsDef =
    { navDate : Date
    , logDate : Date
    , daily : List Cycle
    , monthly : List Cycle
    }


type StatState
    = Loading
    | Loaded StatsDef
    | Error String


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


type alias Settings =
    { rounds : Int
    , activity : Seconds
    , break : Seconds
    , longBreak : Seconds
    , theme : Theme
    , continuity : Continuity
    , spotify : Spotify
    }


type Interval
    = Activity Int
    | Break Int
    | LongBreak Int


type alias Cycle =
    { interval : Interval
    , start : Maybe Posix
    , end : Maybe Posix
    , seconds : Maybe Seconds
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
