module Page.Stats exposing (Msg, StatState, initialState)

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Session
import Time


type StatState
    = Loading
    | Loaded StatsDef


type alias StatsDef =
    { date : Date.Date
    , logs : List Session.Session
    , showLogs : Bool
    }


type Msg
    = GotLogs Decode.Value
    | GoToDate Date.Date
    | GoToMonth Date.Date
    | UpdateSentiment Time.Posix Session.Sentiment
    | ToggleDailyLogs
    | ClearLogs


initialState : StatState
initialState =
    Loading



-- CODECS


decodeLogs : Decode.Decoder { ts : Int, logs : List Session.Session }
decodeLogs =
    Decode.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" Decode.int
        |> Pipeline.required "logs" (Decode.list Session.decodeSession)
