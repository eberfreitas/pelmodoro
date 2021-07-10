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
    , Theme(..)
    , ThemeColors
    )

import Colors exposing (BaseColor)
import Date exposing (Date)
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


type Theme
    = Tomato
    | NightMood
    | Gruvbox
    | Dracula


type alias ThemeColors =
    { background : BaseColor
    , activity : BaseColor
    , break : BaseColor
    , longBreak : BaseColor
    , foreground : BaseColor
    , contrast : BaseColor
    , text : BaseColor
    }


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
