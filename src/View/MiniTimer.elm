module View.MiniTimer exposing (render)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Model exposing (Model)
import Msg exposing (Msg)
import Themes.Theme as Theme


render : Model -> Html Msg
render model =
    let
        totalRun =
            model.intervals |> Model.intervalsTotalRun |> toFloat
    in
    Html.ul
        [ HtmlAttr.css
            [ Css.width <| Css.pct 100
            , Css.displayFlex
            , Css.padding <| Css.rem 0.25
            , Css.listStyle Css.none
            ]
        ]
        (model.intervals
            |> List.indexedMap
                (\index interval ->
                    let
                        sizeInPct =
                            toFloat (Model.intervalSeconds interval) * 100 / totalRun

                        backgroundColor =
                            interval |> Theme.intervalColor model.settings.theme

                        backgroundColor_ =
                            if index >= model.current.index then
                                backgroundColor |> Colors.setAlpha 0.25

                            else
                                backgroundColor
                    in
                    Html.li
                        [ HtmlAttr.css
                            [ Css.width <| Css.pct sizeInPct
                            , Css.height <| Css.rem 0.5
                            , Css.margin <| Css.rem 0.25
                            , Css.borderRadius <| Css.rem 0.25
                            , Css.backgroundColor <| Colors.toCssColor backgroundColor_
                            , Css.overflow Css.hidden
                            ]
                        ]
                        [ if index == model.current.index then
                            let
                                elapsedPct =
                                    Model.currentElapsedPct model.current
                            in
                            Html.div
                                [ HtmlAttr.css
                                    [ Css.width <| Css.pct elapsedPct
                                    , Css.height <| Css.pct 100
                                    , Css.backgroundColor <| Colors.toCssColor backgroundColor
                                    ]
                                ]
                                []

                          else
                            Html.text ""
                        ]
                )
        )
