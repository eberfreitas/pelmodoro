module Page.Stats exposing
    ( Msg
    , StatState
    , initialState
    , loadedWith
    , logsFetchCmd
    , subscriptions
    , unwrapDef
    , update
    )

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Misc
import Ports
import Session
import Time



-- MODEL-ISH


type StatState
    = Loading
    | Loaded StatsDef


type alias StatsDef =
    { date : Date.Date
    , logs : List Session.Session
    , showLogs : Bool
    }



-- UPDATE


type Msg
    = GotLogs Decode.Value
    | GoToDate Date.Date
    | GoToMonth Date.Date
    | UpdateSentiment Time.Posix Session.Sentiment
    | ToggleDailyLogs


update : Msg -> Maybe StatState -> ( Maybe StatState, Cmd msg )
update msg state =
    case ( msg, state ) of
        ( GotLogs _, _ ) ->
            Misc.withCmd state

        ( GoToDate _, _ ) ->
            Misc.withCmd state

        ( GoToMonth _, _ ) ->
            Misc.withCmd state

        ( UpdateSentiment _ _, _ ) ->
            Misc.withCmd state

        ( ToggleDailyLogs, _ ) ->
            Misc.withCmd state



-- HELPERS


initialState : StatState
initialState =
    Loading


unwrapDef : StatState -> Maybe StatsDef
unwrapDef state =
    case state of
        Loaded def ->
            Just def

        _ ->
            Nothing


loadedWith : StatsDef -> StatState
loadedWith =
    Loaded



-- PORTS INTERFACE


type PortAction
    = SetSentiment Time.Posix Session.Sentiment
    | Fetch Time.Posix


encodePortAction : PortAction -> Encode.Value
encodePortAction action =
    case action of
        SetSentiment time sentiment ->
            Encode.object
                [ ( "type", Encode.string "sentiment" )
                , ( "time", Misc.encodePosix time )
                , ( "sentiment", Session.encodeSentiment sentiment )
                ]

        Fetch time ->
            Encode.object
                [ ( "type", Encode.string "fetch" )
                , ( "time", Misc.encodePosix time )
                ]


toPort : PortAction -> Cmd msg
toPort =
    encodePortAction >> Ports.toLog


logsFetchCmd : Time.Posix -> Cmd msg
logsFetchCmd =
    Fetch >> toPort



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.gotFromLog GotLogs



-- CODECS


decodeLogs : Decode.Decoder { ts : Int, logs : List Session.Session }
decodeLogs =
    Decode.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" Decode.int
        |> Pipeline.required "logs" (Decode.list Session.decodeSession)
