module View.Timer exposing (render, secondsToDisplay)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Model exposing (Model)
import Msg exposing (Msg(..))
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttr
import Themes.Theme as Theme
import Types exposing (Current, Interval, Seconds, Theme)
import View.Common as Common


secondsToDisplay : Seconds -> String
secondsToDisplay secs =
    let
        pad num =
            num |> String.fromInt |> String.padLeft 2 '0'
    in
    if secs < 60 then
        "0:" ++ pad secs

    else
        let
            min =
                (toFloat secs / 60) |> floor
        in
        String.fromInt min ++ ":" ++ pad (secs - (min * 60))


renderTimer : Bool -> Int -> Theme -> Current -> Svg Msg
renderTimer playing uptime theme current =
    let
        timerOpacity =
            if playing == True then
                "100"

            else if (uptime |> modBy 2) == 0 then
                "100"

            else
                "0"
    in
    Svg.text_
        [ SvgAttr.x "50%"
        , SvgAttr.y "55%"
        , SvgAttr.textAnchor "middle"
        , SvgAttr.fill (current.cycle.interval |> Theme.intervalColor theme |> Colors.toRgbaString)
        , SvgAttr.fontFamily "Montserrat"
        , SvgAttr.fontSize "36px"
        , SvgAttr.opacity timerOpacity
        ]
        [ Svg.text <| secondsToDisplay (Model.currentSecondsLeft current |> truncate) ]


renderIntervalArcs : Int -> Theme -> Current -> List Interval -> List (Svg msg)
renderIntervalArcs size theme current intervals =
    let
        first ( f, _, _ ) =
            f

        totalRun =
            intervals |> Model.intervalsTotalRun |> toFloat

        strokeWidth =
            8

        centerPoint =
            toFloat size / 2

        radius =
            centerPoint - (strokeWidth / 2)

        arcsOffset =
            3.0
    in
    intervals
        |> List.foldl
            (\interval ( paths, idx, startAngle ) ->
                let
                    intervalSecs =
                        Model.intervalSeconds interval |> toFloat

                    intervalAngle =
                        360.0 * intervalSecs / totalRun

                    endAngle =
                        startAngle + intervalAngle

                    buildArc interval_ opacity_ start_ end_ =
                        Svg.path
                            [ SvgAttr.strokeWidth <| String.fromInt strokeWidth
                            , SvgAttr.strokeLinecap "round"
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke (interval_ |> Theme.intervalColor theme |> Colors.toRgbaString)
                            , SvgAttr.d (describeArc centerPoint centerPoint radius start_ end_)
                            , SvgAttr.opacity opacity_
                            ]
                            []

                    currentArc =
                        if idx == current.index then
                            let
                                elapsedPct =
                                    Model.currentElapsedPct current

                                elapsedIntervalAngle =
                                    (intervalAngle - arcsOffset * 2) * elapsedPct / 100.0

                                startAngle_ =
                                    startAngle + arcsOffset

                                endAngle_ =
                                    startAngle_ + elapsedIntervalAngle
                            in
                            buildArc interval "1" startAngle_ endAngle_

                        else
                            Svg.path [] []

                    opacity =
                        if idx >= current.index then
                            ".35"

                        else
                            "1"
                in
                ( buildArc interval opacity (startAngle + arcsOffset) (endAngle - arcsOffset) :: currentArc :: paths
                , idx + 1
                , endAngle
                )
            )
            ( [], 0, 0 )
        |> first


renderControls : Theme -> Bool -> Html Msg
renderControls theme playing =
    let
        buttonStyle =
            Css.batch
                [ Css.borderStyle Css.none
                , Css.backgroundColor Css.transparent
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.color (theme |> Theme.foregroundColor |> Colors.toCssColor)
                , Css.outline Css.zero
                , Css.cursor Css.pointer
                ]

        button icon msg =
            Html.button
                [ Event.onClick msg, HtmlAttr.css [ buttonStyle ] ]
                [ Common.icon icon ]
    in
    Html.ul
        [ HtmlAttr.css [ Css.listStyle Css.none, Css.displayFlex, Css.marginTop <| Css.rem 1.0 ] ]
        [ Html.li []
            [ if playing then
                button "pause" Pause

              else
                button "play_arrow" Play
            ]
        , Html.li [] [ button "skip_next" Skip ]
        , Html.li [] [ button "restart_alt" Reset ]
        ]


render : Model -> Html Msg
render model =
    let
        svgBaseSize =
            280

        toViewBox =
            List.repeat 2 >> List.map String.fromInt >> String.join " " >> (++) "0 0 "
    in
    Html.div
        [ HtmlAttr.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width <| Css.pct 100.0
            , Css.height <| Css.pct 100.0
            ]
        ]
        [ Svg.svg
            [ SvgAttr.width <| String.fromInt svgBaseSize
            , SvgAttr.height <| String.fromInt svgBaseSize
            , SvgAttr.viewBox <| toViewBox svgBaseSize
            ]
            (renderIntervalArcs svgBaseSize model.settings.theme model.current model.intervals
                ++ [ renderTimer model.playing model.uptime model.settings.theme model.current ]
            )
        , renderControls model.settings.theme model.playing
        ]



-- Functions "stolen" from https://stackoverflow.com/a/18473154/129676


polarToCartesian : Float -> Float -> Float -> Float -> ( Float, Float )
polarToCartesian centerX centerY radius angleInDegrees =
    let
        angleInRadians =
            (angleInDegrees - 90) * pi / 180.0
    in
    ( centerX + (radius * cos angleInRadians)
    , centerY + (radius * sin angleInRadians)
    )


describeArc : Float -> Float -> Float -> Float -> Float -> String
describeArc x y radius startAngle endAngle =
    let
        ( startX, startY ) =
            polarToCartesian x y radius endAngle

        ( endX, endY ) =
            polarToCartesian x y radius startAngle

        largeArcFlag =
            if endAngle - startAngle <= 180.0 then
                "0"

            else
                "1"
    in
    [ "M"
    , String.fromFloat startX
    , String.fromFloat startY
    , "A"
    , String.fromFloat radius
    , String.fromFloat radius
    , "0"
    , largeArcFlag
    , "0"
    , String.fromFloat endX
    , String.fromFloat endY
    ]
        |> String.join " "
