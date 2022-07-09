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

import Calendar
import Color
import Css
import Date
import Elements
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Misc
import Page.MiniTimer as MiniTimer
import Ports
import Session
import Theme
import Theme.Common
import Time
import Tuple.Trio as Trio



-- MODEL


type alias Model a b =
    { a
        | time : Time.Posix
        , zone : Time.Zone
        , settings : { b | theme : Theme.Common.Theme }
        , active : Session.ActiveRound
        , sessions : List Session.RoundType
    }


type State
    = Loading
    | Loaded Def


type alias Def =
    { date : Date.Date
    , logs : List Session.Round
    , showLogs : Bool
    }



-- VIEW


view : Model a b -> State -> Html.Html Msg
view ({ time, zone, settings } as model) state =
    let
        today =
            Date.fromPosix zone time
    in
    Html.div []
        [ MiniTimer.view model
        , Html.div
            [ Attributes.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.maxWidth <| Css.px 520
                ]
            ]
            [ Elements.h1 settings.theme "Statistics"
            , case state of
                Loaded def ->
                    viewLoaded settings.theme zone today def

                _ ->
                    Html.text ""
            ]
        ]


viewLoaded : Theme.Common.Theme -> Time.Zone -> Date.Date -> Def -> Html.Html Msg
viewLoaded theme zone today { date, logs, showLogs } =
    Html.div []
        [ viewCalendar theme zone today date logs
        , viewDailySummary theme zone date logs
        , viewDailyLogs theme showLogs zone date logs
        , viewMonthlySummary theme logs
        , viewHourlyAverages theme zone logs
        ]


viewCalendar :
    Theme.Common.Theme
    -> Time.Zone
    -> Date.Date
    -> Date.Date
    -> List Session.Round
    -> Html.Html Msg
viewCalendar theme zone today date logs =
    let
        averages =
            monthlyAverages zone logs

        cellStyle =
            Css.batch
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.height <| Css.rem 2.3
                ]

        averageForTheDay day =
            averages
                |> List.Extra.find (Tuple.first >> (==) day)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> Misc.flip (/) 100

        cellBgColor average =
            average
                |> Misc.flip Color.setAlpha (theme |> Theme.foregroundColor)
                |> Color.toCssColor

        cellTextColor average =
            if average < 0.5 then
                theme |> Theme.textColor |> Color.toCssColor

            else
                theme |> Theme.contrastColor |> Color.toCssColor

        cellBorder day =
            if day == date then
                Css.border3 (Css.rem 0.15) Css.solid (theme |> Theme.longBreakColor |> Color.toCssColor)

            else
                Css.borderStyle Css.none

        buildDay day =
            let
                average =
                    averageForTheDay day.date

                style =
                    Css.batch
                        [ Css.display Css.block
                        , Css.width <| Css.pct 100
                        , Css.height <| Css.pct 100
                        , Css.backgroundColor (cellBgColor average)
                        , Css.fontSize <| Css.rem 0.75
                        , Css.boxSizing Css.borderBox
                        , Css.color (cellTextColor average)
                        ]

                renderFn =
                    if day.dayDisplay == "  " then
                        Html.div [ Attributes.css [ style ] ]

                    else
                        Html.button
                            [ Attributes.css [ style, Css.cursor Css.pointer, cellBorder day.date ]
                            , if [ LT, EQ ] |> List.member (Date.compare day.date today) then
                                Events.onClick (GoToDate day.date)

                              else
                                Events.onClick NoOp
                            ]
            in
            Html.div
                [ Attributes.css [ cellStyle ] ]
                [ renderFn [ Html.text day.dayDisplay ] ]

        calendar =
            date
                |> Calendar.fromDate Nothing
                |> List.concat
                |> List.map buildDay

        arrowStyle =
            Css.batch
                [ Css.width <| Css.rem 1.5
                , Css.height <| Css.rem 1.5
                , Css.borderStyle Css.none
                , Css.backgroundColor Css.transparent
                , Css.cursor Css.pointer
                , Css.color (theme |> Theme.textColor |> Color.toCssColor)
                ]

        arrow date_ float icon =
            Html.button
                [ Attributes.css [ arrowStyle, Css.float float ]
                , if Date.compare date_ today == LT then
                    Events.onClick <| GoToMonth date_

                  else
                    Events.onClick NoOp
                ]
                [ Elements.icon icon ]

        asFirstDay date_ =
            Date.fromCalendarDate
                (Date.year date_)
                (Date.month date_)
                1

        prevMonth =
            date |> Date.add Date.Months -1 |> asFirstDay

        nextMonth =
            date |> Date.add Date.Months 1 |> asFirstDay
    in
    Html.div
        [ Attributes.css
            [ Css.margin2 (Css.rem 2) Css.auto
            , Css.maxWidth <| Css.px 280
            ]
        ]
        [ Html.div
            [ Attributes.css [ Css.position Css.relative, Css.marginBottom <| Css.rem 1 ] ]
            [ Elements.h2 theme
                (date |> Date.format "MMM / y")
                []
                [ arrow prevMonth Css.left "chevron_left"
                , arrow nextMonth Css.right "chevron_right"
                ]
            ]
        , Html.div
            [ Attributes.css
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns" "repeat(7, 1fr)"
                , Css.property "column-gap" ".2rem"
                , Css.property "row-gap" ".2rem"
                ]
            ]
            ([ "S", "M", "T", "W", "T", "F", "S" ]
                |> List.map
                    (\wd ->
                        Html.div
                            [ Attributes.css [ cellStyle, Css.fontWeight Css.bold ] ]
                            [ Html.div [] [ Html.text wd ] ]
                    )
                |> Misc.flip (++) calendar
            )
        ]


viewSummary : Theme.Common.Theme -> String -> List Session.Round -> Html.Html msg
viewSummary theme label logs =
    if logs == [] then
        Html.text ""

    else
        let
            workLogs =
                logs |> List.filter (.type_ >> Session.isWork)

            breakLogs =
                logs |> List.filter (.type_ >> Session.isAnyBreak)

            aggFn =
                List.foldl
                    (\{ seconds, type_ } ( a, b ) ->
                        ( a + (seconds |> Maybe.withDefault 0)
                        , b + Session.roundSeconds type_
                        )
                    )
                    ( 0, 0 )

            ( workRealSecs, workTotalSecs ) =
                aggFn workLogs

            ( breakRealSecs, breakTotalSecs ) =
                aggFn breakLogs

            workPct =
                workRealSecs * 100 // workTotalSecs

            breakPct =
                breakRealSecs * 100 // breakTotalSecs

            sentiment =
                Session.calculateSentiment logs
        in
        Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
            [ Elements.h2 theme label [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ] []
            , Html.div [ Attributes.css [ Css.textAlign Css.center, Css.marginBottom <| Css.rem 2 ] ]
                [ Elements.h3 theme
                    "Activity time"
                    [ Attributes.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    [ Html.small [] [ Html.text " (in minutes)" ] ]
                , Html.div
                    [ Attributes.css [ Css.marginBottom <| Css.rem 1 ]
                    ]
                    [ Html.text (workRealSecs |> inMinutes |> String.fromInt)
                    , Html.small [] [ Html.text (" (" ++ (workPct |> String.fromInt) ++ "%)") ]
                    ]
                , Elements.h3 theme
                    "Break time"
                    [ Attributes.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    [ Html.small [] [ Html.text " (in minutes)" ] ]
                , Html.div
                    [ Attributes.css [ Css.marginBottom <| Css.rem 1 ]
                    ]
                    [ Html.text (breakRealSecs |> inMinutes |> String.fromInt)
                    , Html.small [] [ Html.text (" (" ++ (breakPct |> String.fromInt) ++ "%)") ]
                    ]
                , Elements.h3 theme
                    "Sentiment"
                    [ Attributes.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    []
                , Html.div [ Attributes.title (Session.sentimentToDisplay sentiment) ]
                    [ Elements.styledIcon
                        [ Css.fontSize <| Css.rem 2 ]
                        (Session.sentimentToIcon sentiment)
                    ]
                ]
            ]


viewDailySummary : Theme.Common.Theme -> Time.Zone -> Date.Date -> List Session.Round -> Html.Html msg
viewDailySummary theme zone date logs =
    viewSummary theme "Daily summary" (dailyLogs zone date logs)


viewDailyLogs : Theme.Common.Theme -> Bool -> Time.Zone -> Date.Date -> List Session.Round -> Html.Html Msg
viewDailyLogs theme show zone selected logs =
    let
        formatToHour t =
            ( t, t )
                |> Tuple.mapBoth
                    (Time.toHour zone >> String.fromInt >> String.padLeft 2 '0')
                    (Time.toMinute zone >> String.fromInt >> String.padLeft 2 'o')
                |> (\( h, m ) -> h ++ ":" ++ m)

        renderSentiment sentiment start =
            let
                opacity msg =
                    sentiment
                        |> Maybe.map
                            (\s ->
                                if UpdateSentiment start s == msg then
                                    Css.opacity <| Css.num 1

                                else
                                    Css.opacity <| Css.num 0.5
                            )
                        |> Maybe.withDefault (Css.opacity <| Css.num 0.5)
            in
            Html.div
                [ Attributes.css
                    [ Css.position Css.absolute
                    , Css.top <| Css.rem 0.25
                    , Css.right <| Css.rem 0.25
                    ]
                ]
                [ Html.ul
                    [ Attributes.css [ Css.listStyle Css.none ] ]
                    ([ ( "Positive", UpdateSentiment start Session.positive, "sentiment_satisfied" )
                     , ( "Neutral", UpdateSentiment start Session.neutral, "sentiment_neutral" )
                     , ( "Negative", UpdateSentiment start Session.negative, "sentiment_dissatisfied" )
                     ]
                        |> List.map
                            (\( label, msg, icon ) ->
                                Html.li [ Attributes.css [ Css.display Css.inlineBlock ] ]
                                    [ Html.button
                                        [ Events.onClick msg
                                        , Attributes.title label
                                        , Attributes.css
                                            [ Css.backgroundColor Css.transparent
                                            , Css.border Css.zero
                                            , Css.padding Css.zero
                                            , Css.margin Css.zero
                                            , Css.cursor Css.pointer
                                            , opacity msg
                                            ]
                                        ]
                                        [ Elements.styledIcon
                                            [ Css.color (theme |> Theme.contrastColor |> Color.toCssColor) ]
                                            icon
                                        ]
                                    ]
                            )
                    )
                ]

        renderRound sentiment round start end seconds =
            let
                innerPct =
                    round
                        |> Session.roundSeconds
                        |> (\t -> 100 * seconds // t)
                        |> String.fromInt

                roundColor =
                    round |> Session.roundToColor theme

                dimmed =
                    roundColor |> Color.setAlpha 0.5 |> Color.toRgbaString

                full =
                    roundColor |> Color.toRgbaString
            in
            Html.div
                [ Attributes.css
                    [ Css.padding <| Css.rem 0.5
                    , Css.position Css.relative
                    , Css.margin2 (Css.rem 0.5) Css.zero
                    , Css.color (theme |> Theme.contrastColor |> Color.toCssColor)
                    , Css.lineHeight <| Css.rem 1
                    , Css.property "background-image"
                        ("linear-gradient(to right, "
                            ++ full
                            ++ ", "
                            ++ full
                            ++ " "
                            ++ innerPct
                            ++ "%, "
                            ++ dimmed
                            ++ " "
                            ++ innerPct
                            ++ "%, "
                            ++ dimmed
                            ++ " 100%)"
                        )
                    ]
                ]
                [ Html.div
                    []
                    [ Html.text (formatToHour start ++ " ➞ " ++ formatToHour end)
                    , if Session.isWork round then
                        renderSentiment sentiment start

                      else
                        Html.text ""
                    ]
                ]

        dailyLogs_ =
            dailyLogs zone selected logs
    in
    if dailyLogs_ /= [] then
        Html.div
            [ Attributes.css
                [ Css.color (theme |> Theme.textColor |> Color.toCssColor)
                , Css.marginBottom <| Css.rem 2
                ]
            ]
            [ Html.div []
                [ Html.div []
                    [ Html.button
                        [ Events.onClick ToggleDailyLogs
                        , Attributes.css
                            [ Css.borderStyle Css.none
                            , Css.backgroundColor <| (theme |> Theme.foregroundColor |> Color.toCssColor)
                            , Css.width <| Css.rem 14
                            , Css.height <| Css.rem 2.5
                            , Css.color <| (theme |> Theme.backgroundColor |> Color.toCssColor)
                            , Css.outline Css.zero
                            , Css.cursor Css.pointer
                            , Css.display Css.block
                            , Css.margin2 Css.zero Css.auto
                            , Css.marginBottom <| Css.rem 2
                            ]
                        ]
                        [ Html.text
                            (if show then
                                "Hide logs"

                             else
                                "Show logs for the day"
                            )
                        ]
                    ]
                , if show then
                    dailyLogs_
                        |> List.sortBy (.start >> Maybe.map Time.posixToMillis >> Maybe.withDefault 0)
                        |> List.filterMap
                            (\{ type_, start, end, seconds, sentiment } ->
                                Maybe.map3
                                    (\s e sc ->
                                        ( s |> Time.posixToMillis |> String.fromInt
                                        , renderRound sentiment type_ s e sc
                                        )
                                    )
                                    start
                                    end
                                    seconds
                            )
                        |> Keyed.node "div" []

                  else
                    Html.text ""
                ]
            ]

    else
        Html.text ""


viewMonthlySummary : Theme.Common.Theme -> List Session.Round -> Html.Html msg
viewMonthlySummary theme logs =
    viewSummary theme "Monthly summary" logs


viewHourlyAverages : Theme.Common.Theme -> Time.Zone -> List Session.Round -> Html.Html msg
viewHourlyAverages theme zone log =
    if log == [] then
        Html.text ""

    else
        let
            averages =
                hourlyAverages zone log

            loggedHours =
                averages |> List.map Trio.first

            hours =
                List.range
                    (loggedHours |> List.minimum |> Maybe.withDefault 0)
                    (loggedHours |> List.maximum |> Maybe.withDefault 23)
        in
        Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
            [ Elements.h2 theme "Most productive hours" [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ] []
            , hours
                |> List.map
                    (\h ->
                        averages
                            |> List.Extra.find (Trio.first >> (==) h)
                            |> Maybe.map
                                (\( _, secs, pct ) ->
                                    Html.div
                                        [ Attributes.css
                                            [ Css.width <| Css.pct 100
                                            , Css.height <| Css.pct pct
                                            , Css.backgroundColor (theme |> Theme.longBreakColor |> Color.toCssColor)
                                            , Css.margin2 Css.zero (Css.rem 0.25)
                                            ]
                                        , Attributes.title (inMinutes secs |> String.fromInt)
                                        ]
                                        []
                                )
                            |> Maybe.withDefault
                                (Html.div
                                    [ Attributes.css
                                        [ Css.margin2 Css.zero (Css.rem 0.25)
                                        , Css.width <| Css.pct 100
                                        ]
                                    ]
                                    [ Html.text "" ]
                                )
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.alignItems Css.flexEnd
                        , Css.height <| Css.rem 5
                        , Css.width <| Css.pct 100
                        ]
                    ]
            , Html.div
                [ Attributes.css
                    [ Css.borderTop <| Css.px 1
                    , Css.borderStyle Css.solid
                    , Css.borderRight Css.zero
                    , Css.borderBottom Css.zero
                    , Css.borderLeft Css.zero
                    , Css.paddingTop <| Css.rem 0.35
                    , Css.fontSize <| Css.rem 0.5
                    , Css.color (theme |> Theme.textColor |> Color.toCssColor)
                    ]
                ]
                (hours
                    |> List.map
                        (\h ->
                            Html.div
                                [ Attributes.css
                                    [ Css.width <| Css.pct 100
                                    , Css.margin2 Css.zero (Css.rem 0.25)
                                    , Css.textAlign Css.center
                                    , Css.overflow Css.hidden
                                    ]
                                ]
                                [ Html.text (h |> String.fromInt |> String.padLeft 2 '0') ]
                        )
                    |> Html.div
                        [ Attributes.css
                            [ Css.displayFlex
                            , Css.width <| Css.pct 100
                            ]
                        ]
                    |> List.singleton
                )
            ]



-- UPDATE


type Msg
    = NoOp
    | GotLogs Decode.Value
    | GoToDate Date.Date
    | GoToMonth Date.Date
    | UpdateSentiment Time.Posix Session.Sentiment
    | ToggleDailyLogs


update : Time.Zone -> Msg -> State -> ( State, Cmd msg )
update zone msg state =
    case msg of
        NoOp ->
            Misc.withCmd state

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


hourlyAverages : Time.Zone -> List Session.Round -> List ( Int, Int, Float )
hourlyAverages zone log =
    let
        aggregate agg { start, seconds } =
            let
                hour =
                    start |> Time.toHour zone
            in
            case agg |> List.Extra.findIndex (Trio.first >> (==) hour) of
                Just idx ->
                    agg |> List.Extra.updateAt idx (\( h, count, secs ) -> ( h, count + 1, secs + seconds ))

                Nothing ->
                    ( hour, 1, seconds ) :: agg

        firstPass =
            log
                |> List.filter (.type_ >> Session.isWork)
                |> List.foldl
                    (\round agg ->
                        round
                            |> Session.materializedRound
                            |> Maybe.map (aggregate agg)
                            |> Maybe.withDefault agg
                    )
                    []
                |> List.map (\( h, count, secs ) -> ( h, secs // count ))

        max =
            firstPass |> List.Extra.maximumBy Tuple.second |> Maybe.map Tuple.second |> Maybe.withDefault 0
    in
    firstPass |> List.map (\( h, secs ) -> ( h, secs, toFloat secs * 100 / toFloat max ))


dailyLogs : Time.Zone -> Date.Date -> List Session.Round -> List Session.Round
dailyLogs zone day logs =
    logs
        |> List.filter
            (.start
                >> Maybe.map (Date.fromPosix zone >> Date.compare day >> (==) EQ)
                >> Maybe.withDefault False
            )


inMinutes : Int -> Int
inMinutes secs =
    secs // 60


monthlyAverages : Time.Zone -> List Session.Round -> List ( Date.Date, Float )
monthlyAverages zone log =
    let
        aggregate agg { start, seconds } =
            let
                date =
                    start |> Date.fromPosix zone
            in
            case agg |> List.Extra.findIndex (Tuple.first >> (==) date) of
                Just idx ->
                    agg |> List.Extra.updateAt idx (\( d, s ) -> ( d, s + seconds ))

                Nothing ->
                    ( date, seconds ) :: agg

        firstPass =
            log
                |> List.filter (.type_ >> Session.isWork)
                |> List.foldl
                    (\round agg ->
                        round
                            |> Session.materializedRound
                            |> Maybe.map (aggregate agg)
                            |> Maybe.withDefault agg
                    )
                    []

        max =
            firstPass |> List.Extra.maximumBy Tuple.second |> Maybe.map Tuple.second |> Maybe.withDefault 0
    in
    firstPass |> List.map (\( date, seconds ) -> ( date, (toFloat seconds * 100) / toFloat max ))


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


decodeLogs : Decode.Decoder { ts : Int, logs : List Session.Round }
decodeLogs =
    Decode.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" Decode.int
        |> Pipeline.required "logs" (Decode.list Session.decodeRound)
