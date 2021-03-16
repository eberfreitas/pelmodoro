module View.Settings exposing (render)

import Colors
import Css
import Helpers
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Model exposing (Model)
import Msg exposing (Msg(..))
import View.MiniTimer as MiniTimer


render : Model -> Html Msg
render ({ settings } as model) =
    let
        labelStyle =
            Css.batch
                [ Css.fontSize <| Css.rem 1.2
                , Css.marginBottom <| Css.rem 1
                , Css.fontWeight <| Css.bold
                ]

        buttonStyle =
            Css.batch
                [ Css.borderStyle Css.none
                , Css.backgroundColor <| (settings.theme |> Colors.foregroundColor |> Colors.toCssColor)
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.color <| (settings.theme |> Colors.backgroundColor |> Colors.toCssColor)
                , Css.outline Css.zero
                , Css.cursor Css.pointer
                ]

        settingDisplayStyle =
            Css.batch
                [ Css.height <| Css.rem 3
                , Css.backgroundColor (settings.theme |> Colors.contrastColor |> Colors.toCssColor)
                , Css.color (settings.theme |> Colors.textColor |> Colors.toCssColor)
                , Css.padding2 (Css.rem 1) (Css.rem 0)
                , Css.width (Css.calc (Css.pct 100) Css.minus (Css.rem 6))
                , Css.textAlign Css.center
                ]

        atLeast target num =
            if num < target then
                target

            else
                num

        atMost target num =
            if num > target then
                target

            else
                num

        inMinutes seconds =
            seconds // 60
    in
    Html.div []
        [ MiniTimer.render model
        , Html.div
            [ HtmlAttr.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.width <| Css.px 280
                ]
            ]
            [ Html.h1
                [ HtmlAttr.css
                    [ Css.fontSize <| Css.rem 2
                    , Css.color (settings.theme |> Colors.foregroundColor |> Colors.toCssColor)
                    , Css.marginBottom <| Css.rem 2
                    , Css.textAlign Css.center
                    ]
                ]
                [ Html.text "Settings" ]
            , Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text "Rounds" ]
                , Html.div
                    [ HtmlAttr.css [ Css.displayFlex ] ]
                    [ Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (settings.rounds - 1 |> atLeast 1 |> ChangeRounds) ] [ Helpers.icon "remove" ]
                    , Html.div
                        [ HtmlAttr.css [ settingDisplayStyle ] ]
                        [ Html.text <| String.fromInt settings.rounds ]
                    , Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (settings.rounds + 1 |> atMost 8 |> ChangeRounds) ] [ Helpers.icon "add" ]
                    ]
                ]
            , Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text "Session duration" ]
                , Html.div
                    [ HtmlAttr.css [ Css.displayFlex ] ]
                    [ Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.activity - 1 |> atLeast 1 |> ChangeActivity) ] [ Helpers.icon "remove" ]
                    , Html.div
                        [ HtmlAttr.css [ settingDisplayStyle ] ]
                        [ Html.text (settings.activity |> inMinutes |> String.fromInt) ]
                    , Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.activity + 1 |> atMost 59 |> ChangeActivity) ] [ Helpers.icon "add" ]
                    ]
                ]
            , Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text "Break duration" ]
                , Html.div
                    [ HtmlAttr.css [ Css.displayFlex ] ]
                    [ Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.break - 1 |> atLeast 1 |> ChangeBreak) ] [ Helpers.icon "remove" ]
                    , Html.div
                        [ HtmlAttr.css [ settingDisplayStyle ] ]
                        [ Html.text (settings.break |> inMinutes |> String.fromInt) ]
                    , Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.break + 1 |> atMost 59 |> ChangeBreak) ] [ Helpers.icon "add" ]
                    ]
                ]
            , Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text "Long break duration" ]
                , Html.div
                    [ HtmlAttr.css [ Css.displayFlex ] ]
                    [ Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.longBreak - 1 |> atLeast 1 |> ChangeLongBreak) ] [ Helpers.icon "remove" ]
                    , Html.div
                        [ HtmlAttr.css [ settingDisplayStyle ] ]
                        [ Html.text (settings.longBreak |> inMinutes |> String.fromInt) ]
                    , Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (inMinutes settings.longBreak + 1 |> atMost 59 |> ChangeLongBreak) ] [ Helpers.icon "add" ]
                    ]
                ]
            ]
        ]
