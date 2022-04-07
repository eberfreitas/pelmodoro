module Page.Timer exposing
    ( Model
    , Msg
    , new
    , update
    , view
    )

import Color
import Css
import Elements
import Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import List.Extra
import Misc
import Page.Spotify as Spotify
import Page.Stats as Stats
import Sessions
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes
import Theme.Common
import Theme.Theme as Theme
import Time
import Tuple.Trio as Trio



-- MODEL


type alias Model =
    { global : Global.Global }


new : Global.Global -> Model
new =
    Model



-- VIEW


view : Model -> Html.Html Msg
view { global } =
    let
        svgBaseSize =
            280

        toViewBox =
            List.repeat 2 >> List.map String.fromInt >> String.join " " >> (++) "0 0 "
    in
    Html.div [ Attributes.css [ Css.width <| Css.pct 100, Css.height <| Css.pct 100 ] ]
        [ Html.div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.width <| Css.pct 100.0
                , Css.height <| Css.pct 100.0
                ]
            ]
            [ Svg.svg
                [ SvgAttributes.width <| String.fromInt svgBaseSize
                , SvgAttributes.height <| String.fromInt svgBaseSize
                , SvgAttributes.viewBox <| toViewBox svgBaseSize
                ]
                (viewSessionsArcs svgBaseSize global.settings.theme global.sessions.active global.sessions.sessions
                    ++ [ viewTimer global.sessions.playing global.sessions.uptime global.settings.theme global.sessions.active ]
                )
            , viewControls global.settings.theme global.sessions.playing
            ]
        , viewSentimentQuery global.settings.theme global.previousRound
        ]


viewSessionsArcs : Int -> Theme.Common.Theme -> Sessions.Active -> List Sessions.SessionDef -> List (Svg.Svg msg)
viewSessionsArcs size theme active sessions =
    let
        totalRun =
            sessions |> Sessions.sessionsTotalRun |> toFloat

        strokeWidth =
            8

        centerPoint =
            toFloat size / 2

        radius =
            centerPoint - (strokeWidth / 2)

        arcsOffset =
            3.0
    in
    sessions
        |> List.foldl
            (\session ( paths, idx, startAngle ) ->
                let
                    sessionSecs =
                        session |> Sessions.sessionSeconds |> toFloat

                    sessionAngle =
                        360.0 * sessionSecs / totalRun

                    endAngle =
                        startAngle + sessionAngle

                    buildArc session_ opacity_ start_ end_ =
                        Svg.path
                            [ SvgAttributes.strokeWidth <| String.fromInt strokeWidth
                            , SvgAttributes.strokeLinecap "round"
                            , SvgAttributes.fill "none"
                            , SvgAttributes.stroke (session_ |> Sessions.toColor theme |> Color.toRgbaString)
                            , SvgAttributes.d (describeArc centerPoint centerPoint radius start_ end_)
                            , SvgAttributes.opacity opacity_
                            ]
                            []

                    activeArc =
                        if idx == active.index then
                            let
                                elapsedPct =
                                    Sessions.elapsedPct active

                                elapsedIntervalAngle =
                                    (sessionAngle - arcsOffset * 2) * elapsedPct / 100.0

                                startAngle_ =
                                    startAngle + arcsOffset

                                endAngle_ =
                                    startAngle_ + elapsedIntervalAngle
                            in
                            buildArc session "1" startAngle_ endAngle_

                        else
                            Svg.path [] []

                    opacity =
                        if idx >= active.index then
                            ".35"

                        else
                            "1"
                in
                ( buildArc session opacity (startAngle + arcsOffset) (endAngle - arcsOffset) :: activeArc :: paths
                , idx + 1
                , endAngle
                )
            )
            ( [], 0, 0 )
        |> Trio.first


viewTimer : Bool -> Int -> Theme.Common.Theme -> Sessions.Active -> Svg.Svg Msg
viewTimer playing uptime theme active =
    let
        timerOpacity =
            if playing then
                "100"

            else if (uptime |> modBy 2) == 0 then
                "100"

            else
                "0"
    in
    Svg.text_
        [ SvgAttributes.x "50%"
        , SvgAttributes.y "55%"
        , SvgAttributes.textAnchor "middle"
        , SvgAttributes.fill (active.session.def |> Sessions.toColor theme |> Color.toRgbaString)
        , SvgAttributes.fontFamily "Montserrat"
        , SvgAttributes.fontSize "36px"
        , SvgAttributes.opacity timerOpacity
        ]
        [ Svg.text <| Sessions.secondsToDisplay (Sessions.secondsLeft active |> truncate) ]


viewControls : Theme.Common.Theme -> Bool -> Html.Html Msg
viewControls theme playing =
    let
        buttonStyle =
            Css.batch
                [ Css.borderStyle Css.none
                , Css.backgroundColor Css.transparent
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.color (theme |> Theme.foregroundColor |> Color.toCssColor)
                , Css.outline Css.zero
                , Css.cursor Css.pointer
                ]

        button icon msg =
            Html.button
                [ Events.onClick msg, Attributes.css [ buttonStyle ] ]
                [ Elements.icon icon ]
    in
    Html.ul
        [ Attributes.css [ Css.listStyle Css.none, Css.displayFlex, Css.marginTop <| Css.rem 1.0 ] ]
        [ Html.li []
            [ if playing then
                button "pause" Pause

              else
                button "play_arrow" Play
            ]
        , Html.li [] [ button "skip_next" Skip ]
        , Html.li [] [ button "restart_alt" Reset ]
        ]


viewSentimentQuery : Theme.Common.Theme -> Maybe Sessions.Session -> Html.Html Msg
viewSentimentQuery theme session =
    session
        |> Maybe.andThen .start
        |> Maybe.map
            (\start ->
                Html.div
                    [ Attributes.css
                        [ Css.position Css.absolute
                        , Css.bottom <| Css.rem 5
                        , Css.left Css.zero
                        , Css.right Css.zero
                        ]
                    ]
                    [ Html.div
                        [ Attributes.css
                            [ Css.maxWidth <| Css.rem 17.5
                            , Css.width <| Css.pct 100
                            , Css.margin2 Css.zero Css.auto
                            ]
                        ]
                        [ Html.div
                            [ Attributes.css
                                [ Css.color (theme |> Theme.textColor |> Color.toCssColor)
                                , Css.fontSize <| Css.rem 0.75
                                , Css.marginBottom <| Css.rem 1
                                , Css.textAlign Css.center
                                ]
                            ]
                            [ Html.strong
                                []
                                [ Html.text "What is your feeling about the last working session?" ]
                            , Html.text " You can set this later on the stats area."
                            ]
                        , Html.ul
                            [ Attributes.css
                                [ Css.displayFlex
                                , Css.justifyContent Css.spaceAround
                                , Css.fontSize <| Css.rem 2
                                , Css.width <| Css.pct 100
                                , Css.listStyle Css.none
                                ]
                            ]
                            ([ ( "Positive", SetSentiment start Sessions.positive, "sentiment_satisfied" )
                             , ( "Neutral", SetSentiment start Sessions.neutral, "sentiment_neutral" )
                             , ( "Negative", SetSentiment start Sessions.negative, "sentiment_dissatisfied" )
                             ]
                                |> List.map
                                    (\( label, msg, icon ) ->
                                        Html.li []
                                            [ Html.button
                                                [ Events.onClick msg
                                                , Attributes.title label
                                                , Attributes.css
                                                    [ Css.backgroundColor Css.transparent
                                                    , Css.border Css.zero
                                                    , Css.padding Css.zero
                                                    , Css.margin Css.zero
                                                    , Css.cursor Css.pointer
                                                    ]
                                                ]
                                                [ Elements.styledIcon
                                                    [ Css.fontSize <| Css.rem 3
                                                    , Css.color (theme |> Theme.textColor |> Color.toCssColor)
                                                    ]
                                                    icon
                                                ]
                                            ]
                                    )
                            )
                        ]
                    ]
            )
        |> Maybe.withDefault (Html.text "")



-- UPDATE


type Msg
    = Play
    | Pause
    | Skip
    | Reset
    | SetSentiment Time.Posix Sessions.Sentiment


update : Msg -> Model -> ( Model, Cmd msg )
update msg { global } =
    let
        { sessions, env, settings } =
            global
    in
    case msg of
        Play ->
            let
                newActive =
                    if sessions.active.elapsed == 0 then
                        Sessions.Active sessions.active.index (Sessions.sessionStart env.time sessions.active.session) 0

                    else
                        sessions.active

                newSessions =
                    { sessions | active = newActive, playing = True }

                cmds =
                    Cmd.batch
                        [ Sessions.saveActive newActive
                        , if Sessions.isWork newActive.session.def then
                            Spotify.playCmd settings.spotify

                          else
                            Cmd.none
                        ]
            in
            { global | sessions = newSessions }
                |> Model
                |> Misc.withCmd
                |> Misc.addCmd cmds

        Pause ->
            { global | sessions = { sessions | playing = False } }
                |> Model
                |> Misc.withCmd
                |> Misc.addCmd (Spotify.pauseCmd settings.spotify)

        Skip ->
            let
                ( nextIndex, nextSessionDef ) =
                    case List.Extra.getAt (sessions.active.index + 1) sessions.sessions of
                        Just next ->
                            ( sessions.active.index + 1, next )

                        Nothing ->
                            ( 0, sessions.sessions |> Sessions.firstSession )

                newActive =
                    Sessions.Active nextIndex (Sessions.newSession nextSessionDef) 0

                newSessions =
                    { sessions | active = newActive, playing = False }

                cmds =
                    Cmd.batch
                        [ Sessions.logSession env.time sessions.active
                        , Sessions.saveActive newActive
                        , Spotify.pauseCmd settings.spotify
                        ]
            in
            { global | sessions = newSessions }
                |> Model
                |> Misc.withCmd
                |> Misc.addCmd cmds

        Reset ->
            let
                newActive =
                    Sessions.newActiveSession sessions.sessions

                newSessions =
                    { sessions | active = newActive, playing = False }
            in
            { global | sessions = newSessions }
                |> Model
                |> Misc.withCmd
                |> Misc.addCmd
                    (Cmd.batch
                        [ Sessions.logSession env.time sessions.active
                        , Sessions.saveActive newActive
                        , Spotify.pauseCmd settings.spotify
                        ]
                    )

        SetSentiment start sentiment ->
            { global | previousRound = Nothing }
                |> Model
                |> Misc.withCmd
                |> Misc.addCmd (Stats.setSentimentCmd start sentiment)



-- HELPERS
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
