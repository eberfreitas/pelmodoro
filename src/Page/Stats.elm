module Page.Stats exposing (Msg)

import Date exposing (Date)
import Json.Decode exposing (Value)
import Session exposing (Sentiment)
import Time exposing (Posix)


type Msg
    = GotLogs Value
    | GoToDate Date
    | GoToMonth Date
    | UpdateSentiment Posix Sentiment
    | ToggleDailyLogs
    | ClearLogs
