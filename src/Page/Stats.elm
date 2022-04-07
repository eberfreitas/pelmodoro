module Page.Stats exposing
    ( Model
    , Msg
    , State
    , logsFetchCmd
    , new
    , setSentimentCmd
    , subscriptions
    , update
    , view
    )

import Calendar
import Color
import Component.MiniTimer as MiniTimer
import Css
import Date
import Elements
import Global
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
import Ports
import Sessions
import Theme.Common
import Theme.Theme as Theme
import Time
import Tuple.Trio as Trio



-- MODEL


type alias Model =
    { global : Global.Global
    , state : State
    }


type State
    = Loading
    | Loaded Def


type alias Def =
    { date : Date.Date
    , logs : List Sessions.Session
    , showLogs : Bool
    }


new : Global.Global -> Model
new global =
    Model global Loading



-- VIEW


view : Model -> Html.Html Msg
view { global, state } =
    let
        { env, settings } =
            global

        today =
            Date.fromPosix env.zone env.time
    in
    Html.div []
        [ MiniTimer.view global
        , Html.div
            [ Attributes.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.maxWidth <| Css.px 520
                ]
            ]
            [ Elements.h1 settings.theme "Statistics"
            , case state of
                Loaded def ->
                    viewLoaded settings.theme env.zone today def

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
    -> List Sessions.Session
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


viewSummary : Theme.Common.Theme -> String -> List Sessions.Session -> Html.Html msg
viewSummary theme label logs =
    if logs == [] then
        Html.text ""

    else
        let
            workLogs =
                logs |> List.filter (.def >> Sessions.isWork)

            breakLogs =
                logs |> List.filter (.def >> Sessions.isAnyBreak)

            aggFn =
                List.foldl
                    (\{ seconds, def } ( a, b ) ->
                        ( a + (seconds |> Maybe.withDefault 0)
                        , b + Sessions.sessionSeconds def
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
                Sessions.calculateSentiment logs
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
                , Html.div [ Attributes.title (Sessions.sentimentToDisplay sentiment) ]
                    [ Elements.styledIcon
                        [ Css.fontSize <| Css.rem 2 ]
                        (Sessions.sentimentToIcon sentiment)
                    ]
                ]
            ]


viewDailySummary : Theme.Common.Theme -> Time.Zone -> Date.Date -> List Sessions.Session -> Html.Html msg
viewDailySummary theme zone date logs =
    viewSummary theme "Daily summary" (dailyLogs zone date logs)


viewDailyLogs : Theme.Common.Theme -> Bool -> Time.Zone -> Date.Date -> List Sessions.Session -> Html.Html Msg
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
                    ([ ( "Positive", UpdateSentiment start Sessions.positive, "sentiment_satisfied" )
                     , ( "Neutral", UpdateSentiment start Sessions.neutral, "sentiment_neutral" )
                     , ( "Negative", UpdateSentiment start Sessions.negative, "sentiment_dissatisfied" )
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

        renderSession sentiment session start end seconds =
            let
                innerPct =
                    session
                        |> Sessions.sessionSeconds
                        |> (\t -> 100 * seconds // t)
                        |> String.fromInt

                sessionColor =
                    session |> Sessions.toColor theme

                dimmed =
                    sessionColor |> Color.setAlpha 0.5 |> Color.toRgbaString

                full =
                    sessionColor |> Color.toRgbaString
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
                    , if Sessions.isWork session then
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
                            (\{ def, start, end, seconds, sentiment } ->
                                Maybe.map3
                                    (\s e sc ->
                                        ( s |> Time.posixToMillis |> String.fromInt
                                        , renderSession sentiment def s e sc
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


viewMonthlySummary : Theme.Common.Theme -> List Sessions.Session -> Html.Html msg
viewMonthlySummary theme logs =
    viewSummary theme "Monthly summary" logs


viewHourlyAverages : Theme.Common.Theme -> Time.Zone -> List Sessions.Session -> Html.Html msg
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
    | UpdateSentiment Time.Posix Sessions.Sentiment
    | ToggleDailyLogs


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ global, state } as model) =
    case msg of
        NoOp ->
            Misc.withCmd model

        GotLogs raw ->
            let
                toDate : Int -> Date.Date
                toDate =
                    Time.millisToPosix >> Date.fromPosix global.env.zone
            in
            case ( Decode.decodeValue decodeLogs raw, state ) of
                ( Ok { ts, logs }, Loading ) ->
                    { model | state = Loaded (Def (ts |> toDate) logs False) } |> Misc.withCmd

                ( Ok { ts, logs }, Loaded def ) ->
                    { model | state = Loaded { def | date = ts |> toDate, logs = logs } } |> Misc.withCmd

                _ ->
                    model |> Misc.withCmd

        GoToDate newDate ->
            { model | state = state |> mapDef (\d -> { d | date = newDate }) } |> Misc.withCmd

        GoToMonth date ->
            let
                ( newState, cmd ) =
                    date
                        |> Date.add Date.Days 1
                        |> Date.toIsoString
                        |> Iso8601.toTime
                        |> Result.map (logsFetchCmd >> Tuple.pair state)
                        |> Result.withDefault (state |> Misc.withCmd)
            in
            ( { model | state = newState }, cmd )

        UpdateSentiment start sentiment ->
            let
                updateSentiment def idx =
                    def.logs
                        |> List.Extra.updateAt idx
                            (\cycle -> { cycle | sentiment = Just sentiment })

                mapper def =
                    let
                        newLogs =
                            def.logs
                                |> List.Extra.findIndex (.start >> (==) (Just start))
                                |> Maybe.map (updateSentiment def)
                                |> Maybe.withDefault def.logs
                    in
                    { def | logs = newLogs }

                newState =
                    state |> mapDef mapper
            in
            { model | state = newState }
                |> Misc.withCmd
                |> Misc.addCmd (setSentimentCmd start sentiment)

        ToggleDailyLogs ->
            { model | state = state |> mapDef (\d -> { d | showLogs = not d.showLogs }) } |> Misc.withCmd



-- HELPERS


hourlyAverages : Time.Zone -> List Sessions.Session -> List ( Int, Int, Float )
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
                |> List.filter (.def >> Sessions.isWork)
                |> List.foldl
                    (\session agg ->
                        session
                            |> Sessions.sessionMaterialized
                            |> Maybe.map (aggregate agg)
                            |> Maybe.withDefault agg
                    )
                    []
                |> List.map (\( h, count, secs ) -> ( h, secs // count ))

        max =
            firstPass |> List.Extra.maximumBy Tuple.second |> Maybe.map Tuple.second |> Maybe.withDefault 0
    in
    firstPass |> List.map (\( h, secs ) -> ( h, secs, toFloat secs * 100 / toFloat max ))


dailyLogs : Time.Zone -> Date.Date -> List Sessions.Session -> List Sessions.Session
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


monthlyAverages : Time.Zone -> List Sessions.Session -> List ( Date.Date, Float )
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
                |> List.filter (.def >> Sessions.isWork)
                |> List.foldl
                    (\session agg ->
                        session
                            |> Sessions.sessionMaterialized
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



-- PORTS INTERFACE


type PortAction
    = SetSentiment Time.Posix Sessions.Sentiment
    | Fetch Time.Posix


encodePortAction : PortAction -> Encode.Value
encodePortAction action =
    case action of
        SetSentiment time sentiment ->
            Encode.object
                [ ( "type", Encode.string "sentiment" )
                , ( "time", Misc.encodePosix time )
                , ( "sentiment", Sessions.encodeSentiment sentiment )
                ]

        Fetch time ->
            Encode.object
                [ ( "type", Encode.string "fetch" )
                , ( "time", Misc.encodePosix time )
                ]


toPort : PortAction -> Cmd msg
toPort =
    encodePortAction >> Ports.toLog


setSentimentCmd : Time.Posix -> Sessions.Sentiment -> Cmd msg
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


decodeLogs : Decode.Decoder { ts : Int, logs : List Sessions.Session }
decodeLogs =
    Decode.succeed (\ts l -> { ts = ts, logs = l })
        |> Pipeline.required "ts" Decode.int
        |> Pipeline.required "logs" (Decode.list Sessions.decodeSession)
