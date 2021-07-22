module Page.Stats exposing
    ( Msg
    , State
    , initialState
    , logsFetchCmd
    , setSentimentCmd
    , subscriptions
    , update
    , view
    )

import Date
import Html.Styled as Html
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
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


update : Time.Zone -> Msg -> State -> ( State, Cmd msg )
update zone msg state =
    case msg of
        GotLogs raw ->
            let
                toDate : Int -> Date.Date
                toDate =
                    Time.millisToPosix >> Date.fromPosix zone
            in
            case ( Decode.decodeValue decodeLogs raw, state ) of
                ( Ok { ts, logs }, Loading ) ->
                    Loaded (Def (ts |> toDate) logs False) |> Misc.withCmd

                ( Ok { ts, logs }, Loaded def ) ->
                    Loaded { def | date = ts |> toDate, logs = logs } |> Misc.withCmd

                _ ->
                    state |> Misc.withCmd

        GoToDate newDate ->
            state
                |> mapDef (\d -> { d | date = newDate })
                |> Misc.withCmd

        GoToMonth date ->
            date
                |> Date.add Date.Days 1
                |> Date.toIsoString
                |> Iso8601.toTime
                |> Result.map (logsFetchCmd >> Tuple.pair state)
                |> Result.withDefault (state |> Misc.withCmd)

        UpdateSentiment start sentiment ->
            state
                |> mapDef
                    (\def ->
                        let
                            newLogs =
                                def.logs
                                    |> List.Extra.findIndex (.start >> (==) (Just start))
                                    |> Maybe.map
                                        (\idx ->
                                            def.logs
                                                |> List.Extra.updateAt idx
                                                    (\cycle -> { cycle | sentiment = Just sentiment })
                                        )
                                    |> Maybe.withDefault def.logs
                        in
                        { def | logs = newLogs }
                    )
                |> Misc.withCmd
                |> Misc.addCmd (setSentimentCmd start sentiment)

        ToggleDailyLogs ->
            state
                |> mapDef (\d -> { d | showLogs = not d.showLogs })
                |> Misc.withCmd



-- HELPERS


mapDef : (Def -> Def) -> State -> State
mapDef map state =
    case state of
        Loaded def ->
            Loaded <| map def

        Loading ->
            Loading


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


setSentimentCmd : Time.Posix -> Session.Sentiment -> Cmd msg
setSentimentCmd start sentiment =
    SetSentiment start sentiment |> toPort


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
