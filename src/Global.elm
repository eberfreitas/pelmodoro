module Global exposing (Global, mapEnv, mapSessions, mapSettings, modelSetFlash, setFlash)

import Env
import Flash as Flash
import Sessions
import Settings


type alias Global =
    { env : Env.Env
    , settings : Settings.Settings
    , sessions : Sessions.Sessions
    , previousRound : Maybe Sessions.Session
    , flash : Flash.Flash
    }


type alias Model a =
    { a | global : Global }


mapSettings : (Settings.Settings -> Settings.Settings) -> Model a -> Model a
mapSettings mapFn ({ global } as model) =
    let
        newSettings =
            mapFn global.settings
    in
    setGlobal { global | settings = newSettings } model


mapSessions : (Sessions.Sessions -> Sessions.Sessions) -> Model a -> Model a
mapSessions mapFn ({ global } as model) =
    let
        newSessions =
            mapFn global.sessions
    in
    setGlobal { global | sessions = newSessions } model


mapEnv : (Env.Env -> Env.Env) -> Model a -> Model a
mapEnv mapFn ({ global } as model) =
    let
        newEnv =
            mapFn global.env
    in
    setGlobal { global | env = newEnv } model


setFlash : Flash.Flash -> Global -> Global
setFlash flash global =
    flash
        |> Maybe.map (\f -> { global | flash = Just f })
        |> Maybe.withDefault global


modelSetFlash : Flash.Flash -> Model a -> Model a
modelSetFlash flash ({ global } as model) =
    let
        newGlobal =
            flash
                |> Maybe.map (\f -> { global | flash = Just f })
                |> Maybe.withDefault global
    in
    setGlobal newGlobal model


setGlobal : Global -> Model a -> Model a
setGlobal global model =
    { model | global = global }
