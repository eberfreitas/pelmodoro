module Page.Stats exposing (Msg, StatState)

import Date exposing (Date)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Session exposing (Sentiment, Session)
import Time exposing (Posix)


type StatState
    = Loading
    | Loaded StatsDef


type alias StatsDef =
    { date : Date
    , logs : List Session
    , showLogs : Bool
    }


type Msg
    = GotLogs Value
    | GoToDate Date
    | GoToMonth Date
    | UpdateSentiment Posix Sentiment
    | ToggleDailyLogs
    | ClearLogs



-- CODECS


decodeLogs : D.Decoder { ts : Int, logs : List Session }
decodeLogs =
    D.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" D.int
        |> Pipeline.required "logs" (D.list Session.decodeSession)
