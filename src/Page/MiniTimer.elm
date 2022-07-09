module Page.MiniTimer exposing (view)

import Color
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Session
import Theme.Common



-- MODEL


type alias Model a b =
    { a
        | sessions : List Session.RoundType
        , active : Session.ActiveRound
        , settings : { b | theme : Theme.Common.Theme }
    }



-- VIEW


view : Model a b -> Html.Html msg
view { sessions, active, settings } =
    let
        totalRun =
            sessions |> Session.roundsTotalRun |> toFloat
    in
    Html.ul
        [ Attributes.css
            [ Css.width <| Css.pct 100
            , Css.displayFlex
            , Css.padding <| Css.rem 0.25
            , Css.listStyle Css.none
            ]
        ]
        (sessions
            |> List.indexedMap
                (\index session ->
                    let
                        sizeInPct =
                            toFloat (Session.roundSeconds session) * 100 / totalRun

                        backgroundColor =
                            session |> Session.roundToColor settings.theme

                        backgroundColor_ =
                            if index >= active.index then
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
                        [ if index == active.index then
                            let
                                elapsedPct =
                                    Session.elapsedPct active
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
