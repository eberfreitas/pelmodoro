module Component.MiniTimer exposing (view)

import Color
import Css
import Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Sessions



-- VIEW


view : Global.Global -> Html.Html msg
view { sessions, settings } =
    let
        totalRun =
            sessions.sessions |> Sessions.sessionsTotalRun |> toFloat
    in
    Html.ul
        [ Attributes.css
            [ Css.width <| Css.pct 100
            , Css.displayFlex
            , Css.padding <| Css.rem 0.25
            , Css.listStyle Css.none
            ]
        ]
        (sessions.sessions
            |> List.indexedMap
                (\index session ->
                    let
                        sizeInPct =
                            toFloat (Sessions.sessionSeconds session) * 100 / totalRun

                        backgroundColor =
                            session |> Sessions.toColor settings.theme

                        backgroundColor_ =
                            if index >= sessions.active.index then
                                backgroundColor |> Color.setAlpha 0.25

                            else
                                backgroundColor
                    in
                    Html.li
                        [ Attributes.css
                            [ Css.width <| Css.pct sizeInPct
                            , Css.height <| Css.rem 0.5
                            , Css.margin <| Css.rem 0.25
                            , Css.borderRadius <| Css.rem 0.25
                            , Css.backgroundColor <| Color.toCssColor backgroundColor_
                            , Css.overflow Css.hidden
                            ]
                        ]
                        [ if index == sessions.active.index then
                            let
                                elapsedPct =
                                    Sessions.elapsedPct sessions.active
                            in
                            Html.div
                                [ Attributes.css
                                    [ Css.width <| Css.pct elapsedPct
                                    , Css.height <| Css.pct 100
                                    , Css.backgroundColor <| Color.toCssColor backgroundColor
                                    ]
                                ]
                                []

                          else
                            Html.text ""
                        ]
                )
        )
