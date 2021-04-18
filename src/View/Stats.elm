module View.Stats exposing (render)

import Calendar
import Colors
import Css
import Date exposing (Date, Unit(..))
import Helpers
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (Page(..), StatState(..), StatsDef, Theme)
import View.Common as Common
import View.MiniTimer as MiniTimer


render : Model -> Html Msg
render ({ settings } as model) =
    Html.div []
        [ MiniTimer.render model
        , Html.div
            [ HtmlAttr.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.width <| Css.px 280
                ]
            ]
            [ Common.header settings.theme "Statistics"
            , case model.page of
                StatsPage (Loaded def) ->
                    renderLoaded settings.theme def

                _ ->
                    Html.text ""
            ]
        ]


renderLoaded : Theme -> StatsDef -> Html Msg
renderLoaded theme def =
    Html.div []
        [ renderCalendar theme def.navDate def.logDate
        ]


renderCalendar : Theme -> Date -> Date -> Html Msg
renderCalendar theme navDate logDate =
    let
        cellStyle =
            Css.batch
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.height <| Css.rem 2.3
                ]

        buildDay _ day =
            Html.div
                [ HtmlAttr.css [ cellStyle ] ]
                [ Html.button
                    [ HtmlAttr.css
                        [ Css.borderStyle Css.none
                        , Css.display Css.block
                        , Css.width <| Css.pct 100
                        , Css.height <| Css.pct 100
                        , Css.cursor Css.pointer
                        , Css.backgroundColor Css.transparent
                        , Css.fontSize <| Css.rem 0.75
                        , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
                        ]
                    ]
                    [ Html.text day.dayDisplay ]
                ]

        calendar =
            navDate
                |> Calendar.fromDate Nothing
                |> List.map (\week -> week |> List.map (buildDay logDate))
                |> List.concat

        arrowStyle =
            Css.batch
                [ Css.width <| Css.rem 1.5
                , Css.height <| Css.rem 1.5
                , Css.borderStyle Css.none
                , Css.backgroundColor Css.transparent
                , Css.cursor Css.pointer
                , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
                ]

        arrow date float icon =
            Html.button
                [ HtmlAttr.css [ arrowStyle, Css.float float ]
                , Event.onClick <| ChangeNavDate date
                ]
                [ Common.icon icon ]

        prevMonth =
            navDate |> Date.add Months -1

        nextMonth =
            navDate |> Date.add Months 1
    in
    Html.div []
        [ Html.div
            [ HtmlAttr.css [ Css.position Css.relative, Css.marginBottom <| Css.rem 1 ] ]
            [ Html.h2
                [ HtmlAttr.css
                    [ Css.fontSize <| Css.rem 1.5
                    , Css.textAlign <| Css.center
                    , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
                    ]
                ]
                [ Html.text (navDate |> Date.format "MMM / y")
                , arrow prevMonth Css.left "chevron_left"
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
