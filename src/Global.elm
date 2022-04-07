module Global exposing (Global, mapSettings, setFlash)

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

        newGlobal =
            { global | settings = newSettings }
    in
    setGlobal newGlobal model


setFlash : Flash.Flash -> Model a -> Model a
setFlash flash ({ global } as model) =
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
