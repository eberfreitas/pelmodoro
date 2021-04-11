module Types exposing
    ( Continuity(..)
    , Current
    , Cycle
    , Interval(..)
    , Log
    , Page(..)
    , Seconds
    , Settings
    , Spotify(..)
    , SpotifyPlaylist
    , Theme(..)
    )

import Time exposing (Posix)


type Page
    = TimerPage
    | SettingsPage
    | StatsPage
    | CreditsPage


type alias Seconds =
    Int


type Theme
    = LightTheme
    | DarkTheme


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


type alias Log =
    List Cycle
