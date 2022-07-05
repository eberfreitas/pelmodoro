module Context exposing (Context, Msg(..), update)

import Env
import Flash as Flash
import Sessions
import Settings


type alias Context =
    { env : Env.Env
    , settings : Settings.Settings
    , sessions : Sessions.Sessions
    , previousRound : Maybe Sessions.Session
    , flash : Flash.Flash
    , pageTitle : String
    }


type Msg
    = NoContextOp
    | UpdateTitle String


update : Msg -> Context -> Context
update msg context =
    case msg of
        NoContextOp ->
            context

        UpdateTitle newTitle ->
            { context | pageTitle = newTitle }



-- setFlash : Flash.Flash -> Global -> Global
-- setFlash flash global =
--     flash
--         |> Maybe.map (\f -> { global | flash = Just f })
--         |> Maybe.withDefault global
