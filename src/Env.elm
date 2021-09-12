module Env exposing (Env)

import Browser.Navigation as Navigation
import Time


type alias Env =
    { zone : Time.Zone
    , time : Time.Posix
    , key : Navigation.Key
    }
