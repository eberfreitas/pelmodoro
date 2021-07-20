module View.Stats exposing (render)

import Calendar
import Colors
import Css
import Date exposing (Date, Unit(..))
import Helpers
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Html.Styled.Keyed as Keyed
import List.Extra as ListEx
import Model exposing (Model)
import Msg exposing (Msg(..))
import Themes.Theme as Theme
import Themes.Types exposing (Theme)
import Time exposing (Zone)
import Tools
import Tuple.Trio as Trio
import Types exposing (Cycle, Interval(..), Page(..), Sentiment(..), StatState(..), StatsDef)
import View.Common as Common
import View.MiniTimer as MiniTimer


inMinutes : Int -> Int
inMinutes x =
    x // 60


dailyLogs : Zone -> Date -> List Cycle -> List Cycle
dailyLogs zone day logs =
    logs
        |> List.filter
            (.start
                >> Maybe.map (Date.fromPosix zone >> Date.compare day >> (==) EQ)
                >> Maybe.withDefault False
            )


render : Model -> Html Msg
render ({ settings, time, zone } as model) =
    let
        today =
            Date.fromPosix zone time
    in
    Html.div []
        [ MiniTimer.render model
        , Html.div
            [ HtmlAttr.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.maxWidth <| Css.px 520
                ]
            ]
            [ Common.h1 settings.theme "Statistics"
            , case model.page of
                StatsPage (Loaded def_) ->
                    renderLoaded zone today settings.theme def_

                _ ->
                    Html.text ""
            ]
        ]


calculateSentiment : List Cycle -> Sentiment
calculateSentiment =
    List.filter (.interval >> Model.intervalIsActivity)
        >> List.map (.sentiment >> Maybe.withDefault Neutral)
        >> List.foldl
            (\sentiment ( positive, neutral, negative ) ->
                case sentiment of
                    Positive ->
                        ( positive + 1, neutral, negative )

                    Neutral ->
                        ( positive, neutral + 1, negative )

                    Negative ->
                        ( positive, neutral, negative + 1 )
            )
            ( 0, 0, 0 )
        >> (\( positive, neutral, negative ) ->
                if negative >= neutral && negative >= positive then
                    Negative

                else if neutral >= positive && neutral >= negative then
                    Neutral

                else
                    Positive
           )


sentimentToIcon : Sentiment -> String
sentimentToIcon sentiment =
    case sentiment of
        Positive ->
            "sentiment_satisfied"

        Neutral ->
            "sentiment_neutral"

        Negative ->
            "sentiment_dissatisfied"


renderLoaded : Zone -> Date -> Theme -> StatsDef -> Html Msg
renderLoaded zone today theme { date, logs, showLogs } =
    Html.div []
        [ renderCalendar zone theme today date logs
        , renderDailySummary zone theme date logs
        , renderDailyLogs showLogs zone theme date logs
        , renderMonthlySummary theme logs
        , renderHourlyAverages zone theme logs
        ]


renderDailySummary : Zone -> Theme -> Date -> List Cycle -> Html msg
renderDailySummary zone theme date logs =
    renderSummary "Daily summary" theme (dailyLogs zone date logs)


renderMonthlySummary : Theme -> List Cycle -> Html msg
renderMonthlySummary theme logs =
    renderSummary "Monthly summary" theme logs


renderSummary : String -> Theme -> List Cycle -> Html msg
renderSummary label theme logs =
    if logs == [] then
        Html.text ""

    else
        let
            activityLogs =
                logs |> List.filter (.interval >> Model.intervalIsActivity)

            breakLogs =
                logs |> List.filter (.interval >> Model.intervalIsBreak)

            aggFn =
                List.foldl
                    (\{ seconds, interval } ( a, b ) ->
                        ( a + (seconds |> Maybe.withDefault 0)
                        , b + Model.intervalSeconds interval
                        )
                    )
                    ( 0, 0 )

            ( activityRealSecs, activityTotalSecs ) =
                aggFn activityLogs

            ( breakRealSecs, breakTotalSecs ) =
                aggFn breakLogs

            activityPct =
                activityRealSecs * 100 // activityTotalSecs

            breakPct =
                breakRealSecs * 100 // breakTotalSecs

            sentiment =
                calculateSentiment logs
        in
        Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
            [ Common.h2 theme label [ HtmlAttr.css [ Css.marginBottom <| Css.rem 1 ] ] []
            , Html.div [ HtmlAttr.css [ Css.textAlign Css.center, Css.marginBottom <| Css.rem 2 ] ]
                [ Common.h3 theme
                    "Activity time"
                    [ HtmlAttr.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    [ Html.small [] [ Html.text "(in minutes)" ] ]
                , Html.div
                    [ HtmlAttr.css [ Css.marginBottom <| Css.rem 1 ]
                    ]
                    [ Html.text (activityRealSecs |> inMinutes |> String.fromInt)
                    , Html.small [] [ Html.text (" (" ++ (activityPct |> String.fromInt) ++ "%)") ]
                    ]
                , Common.h3 theme
                    "Break time"
                    [ HtmlAttr.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    [ Html.small [] [ Html.text "(in minutes)" ] ]
                , Html.div
                    [ HtmlAttr.css [ Css.marginBottom <| Css.rem 1 ]
                    ]
                    [ Html.text (breakRealSecs |> inMinutes |> String.fromInt)
                    , Html.small [] [ Html.text (" (" ++ (breakPct |> String.fromInt) ++ "%)") ]
                    ]
                , Common.h3 theme
                    "Sentiment"
                    [ HtmlAttr.css [ Css.marginBottom <| Css.rem 0.5 ] ]
                    []
                , Html.div [ HtmlAttr.title (Tools.sentimentToDisplay sentiment) ]
                    [ Common.styledIcon
                        [ Css.fontSize <| Css.rem 2 ]
                        (sentimentToIcon sentiment)
                    ]
                ]
            ]


renderHourlyAverages : Zone -> Theme -> List Cycle -> Html msg
renderHourlyAverages zone theme log =
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
        Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
            [ Common.h2 theme "Most productive hours" [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ] []
            , hours
                |> List.map
                    (\h ->
                        averages
                            |> ListEx.find (Trio.first >> (==) h)
                            |> Maybe.map
                                (\( _, secs, pct ) ->
                                    Html.div
                                        [ HtmlAttr.css
                                            [ Css.width <| Css.pct 100
                                            , Css.height <| Css.pct pct
                                            , Css.backgroundColor (theme |> Theme.longBreakColor |> Colors.toCssColor)
                                            , Css.margin2 Css.zero (Css.rem 0.25)
                                            ]
                                        , HtmlAttr.title (inMinutes secs |> String.fromInt)
                                        ]
                                        []
                                )
                            |> Maybe.withDefault
                                (Html.div
                                    [ HtmlAttr.css
                                        [ Css.margin2 Css.zero (Css.rem 0.25)
                                        , Css.width <| Css.pct 100
                                        ]
                                    ]
                                    [ Html.text "" ]
                                )
                    )
                |> Html.div
                    [ HtmlAttr.css
                        [ Css.displayFlex
                        , Css.alignItems Css.flexEnd
                        , Css.height <| Css.rem 5
                        , Css.width <| Css.pct 100
                        ]
                    ]
            , Html.div
                [ HtmlAttr.css
                    [ Css.borderTop <| Css.px 1
                    , Css.borderStyle Css.solid
                    , Css.borderRight Css.zero
                    , Css.borderBottom Css.zero
                    , Css.borderLeft Css.zero
                    , Css.paddingTop <| Css.rem 0.35
                    , Css.fontSize <| Css.rem 0.5
                    , Css.color (theme |> Theme.textColor |> Colors.toCssColor)
                    ]
                ]
                (hours
                    |> List.map
                        (\h ->
                            Html.div
                                [ HtmlAttr.css
                                    [ Css.width <| Css.pct 100
                                    , Css.margin2 Css.zero (Css.rem 0.25)
                                    , Css.textAlign Css.center
                                    , Css.overflow Css.hidden
                                    ]
                                ]
                                [ Html.text (h |> String.fromInt |> String.padLeft 2 '0') ]
                        )
                    |> Html.div
                        [ HtmlAttr.css
                            [ Css.displayFlex
                            , Css.width <| Css.pct 100
                            ]
                        ]
                    |> List.singleton
                )
            ]


renderDailyLogs : Bool -> Zone -> Theme -> Date -> List Cycle -> Html Msg
renderDailyLogs show zone theme selected logs =
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
                [ HtmlAttr.css
                    [ Css.position Css.absolute
                    , Css.top <| Css.rem 0.25
                    , Css.right <| Css.rem 0.25
                    ]
                ]
                [ Html.ul
                    [ HtmlAttr.css [ Css.listStyle Css.none ] ]
                    ([ ( "Positive", UpdateSentiment start Positive, "sentiment_satisfied" )
                     , ( "Neutral", UpdateSentiment start Neutral, "sentiment_neutral" )
                     , ( "Negative", UpdateSentiment start Negative, "sentiment_dissatisfied" )
                     ]
                        |> List.map
                            (\( label, msg, icon ) ->
                                Html.li [ HtmlAttr.css [ Css.display Css.inlineBlock ] ]
                                    [ Html.button
                                        [ Event.onClick msg
                                        , HtmlAttr.title label
                                        , HtmlAttr.css
                                            [ Css.backgroundColor Css.transparent
                                            , Css.border Css.zero
                                            , Css.padding Css.zero
                                            , Css.margin Css.zero
                                            , Css.cursor Css.pointer
                                            , opacity msg
                                            ]
                                        ]
                                        [ Common.styledIcon
                                            [ Css.color (theme |> Theme.contrastColor |> Colors.toCssColor) ]
                                            icon
                                        ]
                                    ]
                            )
                    )
                ]

        renderCycle sentiment interval start end seconds =
            let
                innerPct =
                    interval
                        |> Model.intervalSeconds
                        |> (\t -> 100 * seconds // t)
                        |> String.fromInt

                intervalColor =
                    interval |> Theme.intervalColor theme

                dimmed =
                    intervalColor |> Colors.setAlpha 0.5 |> Colors.toRgbaString

                full =
                    intervalColor |> Colors.toRgbaString
            in
            Html.div
                [ HtmlAttr.css
                    [ Css.padding <| Css.rem 0.5
                    , Css.position Css.relative
                    , Css.margin2 (Css.rem 0.5) Css.zero
                    , Css.color (theme |> Theme.contrastColor |> Colors.toCssColor)
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
                    , case interval of
                        Activity _ ->
                            renderSentiment sentiment start

                        _ ->
                            Html.text ""
                    ]
                ]

        dailyLogs_ =
            dailyLogs zone selected logs
    in
    Html.div
        [ HtmlAttr.css
            [ Css.color (theme |> Theme.textColor |> Colors.toCssColor)
            , Css.marginBottom <| Css.rem 2
            ]
        ]
        [ Html.div []
            [ Html.div []
                [ Html.button
                    [ Event.onClick ToggleLogs
                    , HtmlAttr.css
                        [ Css.borderStyle Css.none
                        , Css.backgroundColor <| (theme |> Theme.foregroundColor |> Colors.toCssColor)
                        , Css.width <| Css.rem 14
                        , Css.height <| Css.rem 2.5
                        , Css.color <| (theme |> Theme.backgroundColor |> Colors.toCssColor)
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
            , case ( show, dailyLogs_ ) of
                ( True, [] ) ->
                    Html.div
                        [ HtmlAttr.css [ Css.textAlign Css.center ] ]
                        [ Html.text "Nothing was logged this day" ]

                ( True, log ) ->
                    log
                        |> List.sortBy (.start >> Maybe.map Time.posixToMillis >> Maybe.withDefault 0)
                        |> List.filterMap
                            (\{ interval, start, end, seconds, sentiment } ->
                                Maybe.map3
                                    (\s e sc ->
                                        ( s |> Time.posixToMillis |> String.fromInt
                                        , renderCycle sentiment interval s e sc
                                        )
                                    )
                                    start
                                    end
                                    seconds
                            )
                        |> Keyed.node "div" []

                _ ->
                    Html.text ""
            ]
        ]


hourlyAverages : Zone -> List Cycle -> List ( Int, Int, Float )
hourlyAverages zone log =
    let
        aggregate agg { start, seconds } =
            let
                hour =
                    start |> Time.toHour zone
            in
            case agg |> ListEx.findIndex (Trio.first >> (==) hour) of
                Just idx ->
                    agg |> ListEx.updateAt idx (\( h, count, secs ) -> ( h, count + 1, secs + seconds ))

                Nothing ->
                    ( hour, 1, seconds ) :: agg

        firstPass =
            log
                |> List.filter (.interval >> Model.intervalIsActivity)
                |> List.foldl
                    (\cycle agg ->
                        cycle
                            |> Model.cycleMaterialized
                            |> Maybe.map (aggregate agg)
                            |> Maybe.withDefault agg
                    )
                    []
                |> List.map (\( h, count, secs ) -> ( h, secs // count ))

        max =
            firstPass |> ListEx.maximumBy Tuple.second |> Maybe.map Tuple.second |> Maybe.withDefault 0
    in
    firstPass |> List.map (\( h, secs ) -> ( h, secs, toFloat secs * 100 / toFloat max ))


monthlyAverages : Zone -> List Cycle -> List ( Date, Float )
monthlyAverages zone log =
    let
        aggregate agg { start, seconds } =
            let
                date =
                    start |> Date.fromPosix zone
            in
            case agg |> ListEx.findIndex (Tuple.first >> (==) date) of
                Just idx ->
                    agg |> ListEx.updateAt idx (\( d, s ) -> ( d, s + seconds ))

                Nothing ->
                    ( date, seconds ) :: agg

        firstPass =
            log
                |> List.filter (.interval >> Model.intervalIsActivity)
                |> List.foldl
                    (\cycle agg ->
                        cycle
                            |> Model.cycleMaterialized
                            |> Maybe.map (aggregate agg)
                            |> Maybe.withDefault agg
                    )
                    []

        max =
            firstPass |> ListEx.maximumBy Tuple.second |> Maybe.map Tuple.second |> Maybe.withDefault 0
    in
    firstPass |> List.map (\( date, seconds ) -> ( date, (toFloat seconds * 100) / toFloat max ))


renderCalendar : Zone -> Theme -> Date -> Date -> List Cycle -> Html Msg
renderCalendar zone theme today date logs =
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
                |> ListEx.find (Tuple.first >> (==) day)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> Helpers.flip (/) 100

        cellBgColor average =
            average
                |> Helpers.flip Colors.setAlpha (theme |> Theme.foregroundColor)
                |> Colors.toCssColor

        cellTextColor average =
            if average < 0.5 then
                theme |> Theme.textColor |> Colors.toCssColor

            else
                theme |> Theme.contrastColor |> Colors.toCssColor

        cellBorder day =
            if day == date then
                Css.border3 (Css.rem 0.15) Css.solid (theme |> Theme.longBreakColor |> Colors.toCssColor)

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
                        Html.div [ HtmlAttr.css [ style ] ]

                    else
                        Html.button
                            [ HtmlAttr.css [ style, Css.cursor Css.pointer, cellBorder day.date ]
                            , if [ LT, EQ ] |> List.member (Date.compare day.date today) then
                                Event.onClick (ChangeLogDate day.date)

                              else
                                Event.onClick NoOp
                            ]
            in
            Html.div
                [ HtmlAttr.css [ cellStyle ] ]
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
                , Css.color (theme |> Theme.textColor |> Colors.toCssColor)
                ]

        arrow date_ float icon =
            Html.button
                [ HtmlAttr.css [ arrowStyle, Css.float float ]
                , if Date.compare date_ today == LT then
                    Event.onClick <| ChangeLogMonth date_

                  else
                    Event.onClick NoOp
                ]
                [ Common.icon icon ]

        asFirstDay date_ =
            Date.fromCalendarDate
                (Date.year date_)
                (Date.month date_)
                1

        prevMonth =
            date |> Date.add Months -1 |> asFirstDay

        nextMonth =
            date |> Date.add Months 1 |> asFirstDay
    in
    Html.div
        [ HtmlAttr.css
            [ Css.margin2 (Css.rem 2) Css.auto
            , Css.maxWidth <| Css.px 280
            ]
        ]
        [ Html.div
            [ HtmlAttr.css [ Css.position Css.relative, Css.marginBottom <| Css.rem 1 ] ]
            [ Common.h2 theme
                (date |> Date.format "MMM / y")
                []
                [ arrow prevMonth Css.left "chevron_left"
                , arrow nextMonth Css.right "chevron_right"
                ]
            ]
        , Html.div
            [ HtmlAttr.css
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
                            [ HtmlAttr.css [ cellStyle, Css.fontWeight Css.bold ] ]
                            [ Html.div [] [ Html.text wd ] ]
                    )
                |> Helpers.flip (++) calendar
            )
        ]
