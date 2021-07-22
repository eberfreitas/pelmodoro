module Page.Stats exposing
    ( Msg
    , State
    , initialState
    , logsFetchCmd
    , subscriptions
    , update
    , view
    )

import Date
import Html.Styled as Html
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Misc
import Ports
import Session
import Time



-- MODEL-ISH


type State
    = Loading
    | Loaded Def


type alias Def =
    { date : Date.Date
    , logs : List Session.Session
    , showLogs : Bool
    }



-- VIEW


view : a -> Html.Html msg
view _ =
    Html.text ""



-- UPDATE


type Msg
    = GotLogs Decode.Value
    | GoToDate Date.Date
    | GoToMonth Date.Date
    | UpdateSentiment Time.Posix Session.Sentiment
    | ToggleDailyLogs


update : Msg -> State -> ( State, Cmd msg )
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


initialState : State
initialState =
    Loading



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
