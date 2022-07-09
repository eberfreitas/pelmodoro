module Page.Timer exposing (Msg, secondsToDisplay, subscriptions, update, view)

import Color
import Css
import Elements
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Misc
import Page.Flash as Flash
import Page.Spotify as Spotify
import Page.Stats as Stats
import Ports
import Session
import Settings
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes
import Theme
import Theme.Common
import Time
import Tuple.Trio as Trio



-- MODEL


type alias Model a =
    { a
        | time : Time.Posix
        , playing : Bool
        , active : Session.Active
        , settings : Settings.Settings
        , sessions : List Session.SessionDef
        , uptime : Int
        , flash : Maybe Flash.FlashMsg
        , sentimentSession : Maybe Session.Session
    }


type alias EvalResult msg =
    { active : Session.Active
    , playing : Bool
    , flash : Maybe Flash.FlashMsg
    , cmd : Cmd msg
    , sentimentSession : Maybe Session.Session
    }



-- VIEW


view : Model a -> Html.Html Msg
view model =
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
                (viewSessionsArcs svgBaseSize model.settings.theme model.active model.sessions
                    ++ [ viewTimer model.playing model.uptime model.settings.theme model.active ]
                )
            , viewControls model.settings.theme model.playing
            ]
        , viewSentimentQuery model.settings.theme model.sentimentSession
        ]


viewSessionsArcs : Int -> Theme.Common.Theme -> Session.Active -> List Session.SessionDef -> List (Svg.Svg msg)
viewSessionsArcs size theme active sessions =
    let
        totalRun =
            sessions |> Session.sessionsTotalRun |> toFloat

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
                        session |> Session.sessionSeconds |> toFloat

                    sessionAngle =
                        360.0 * sessionSecs / totalRun

                    endAngle =
                        startAngle + sessionAngle

                    buildArc session_ opacity_ start_ end_ =
                        Svg.path
                            [ SvgAttributes.strokeWidth <| String.fromInt strokeWidth
                            , SvgAttributes.strokeLinecap "round"
                            , SvgAttributes.fill "none"
                            , SvgAttributes.stroke (session_ |> Session.toColor theme |> Color.toRgbaString)
                            , SvgAttributes.d (describeArc centerPoint centerPoint radius start_ end_)
                            , SvgAttributes.opacity opacity_
                            ]
                            []

                    activeArc =
                        if idx == active.index then
                            let
                                elapsedPct =
                                    Session.elapsedPct active

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


viewTimer : Bool -> Int -> Theme.Common.Theme -> Session.Active -> Svg.Svg Msg
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
        , SvgAttributes.fill (active.session.def |> Session.toColor theme |> Color.toRgbaString)
        , SvgAttributes.fontFamily "Montserrat"
        , SvgAttributes.fontSize "36px"
        , SvgAttributes.opacity timerOpacity
        ]
        [ Svg.text <| secondsToDisplay (Session.secondsLeft active |> truncate) ]


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


viewSentimentQuery : Theme.Common.Theme -> Maybe Session.Session -> Html.Html Msg
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
                            ([ ( "Positive", SetSentiment start Session.positive, "sentiment_satisfied" )
                             , ( "Neutral", SetSentiment start Session.neutral, "sentiment_neutral" )
                             , ( "Negative", SetSentiment start Session.negative, "sentiment_dissatisfied" )
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
    = Tick Decode.Value
    | Play
    | Pause
    | Skip
    | Reset
    | SetSentiment Time.Posix Session.Sentiment


update : Msg -> Model a -> ( Model a, Cmd msg )
update msg ({ settings, active, time, sessions } as model) =
    case msg of
        Tick raw ->
            case Decode.decodeValue Decode.int raw of
                Ok millis ->
                    model |> tick (Time.millisToPosix millis)

                Err _ ->
                    Misc.withCmd model

        Play ->
            let
                newActive =
                    if active.elapsed == 0 then
                        Session.Active active.index (Session.sessionStart time active.session) 0

                    else
                        active

                cmds =
                    Cmd.batch
                        [ Session.saveActive newActive
                        , if Session.isWork newActive.session.def then
                            Spotify.play settings.spotify

                          else
                            Cmd.none
                        ]
            in
            { model | playing = True, active = newActive }
                |> Misc.withCmd
                |> Misc.addCmd cmds

        Pause ->
            { model | playing = False }
                |> Misc.withCmd
                |> Misc.addCmd (Spotify.pause settings.spotify)

        Skip ->
            let
                ( nextIndex, nextSessionDef ) =
                    case List.Extra.getAt (active.index + 1) model.sessions of
                        Just next ->
                            ( active.index + 1, next )

                        Nothing ->
                            ( 0, model.sessions |> Session.firstSession )

                newActive =
                    Session.Active nextIndex (Session.newSession nextSessionDef) 0

                cmds =
                    Cmd.batch
                        [ Session.logSession time active
                        , Session.saveActive newActive
                        , Spotify.pause settings.spotify
                        ]
            in
            { model | active = newActive, playing = False }
                |> Misc.withCmd
                |> Misc.addCmd cmds

        Reset ->
            let
                newActive =
                    Session.newActiveSession sessions
            in
            { model | active = newActive, playing = False }
                |> Misc.withCmd
                |> Misc.addCmd
                    (Cmd.batch
                        [ Session.logSession time active
                        , Session.saveActive newActive
                        , Spotify.pause settings.spotify
                        ]
                    )

        SetSentiment start sentiment ->
            { model | sentimentSession = Nothing }
                |> Misc.withCmd
                |> Misc.addCmd (Stats.setSentimentCmd start sentiment)



-- HELPERS


secondsToDisplay : Int -> String
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


rollActiveSession : Time.Posix -> Int -> Settings.Flow -> List Session.SessionDef -> ( Session.Active, Bool )
rollActiveSession now nextIndex flow sessions =
    let
        firstSession_ =
            sessions |> Session.firstSession

        nextActive =
            case sessions |> List.Extra.getAt nextIndex of
                Nothing ->
                    Session.Active 0 (Session.newSession firstSession_) 0

                Just nextSession ->
                    Session.Active nextIndex (Session.newSession nextSession) 0
    in
    if Settings.shouldKeepPlaying nextActive.index flow then
        ( { nextActive | session = Session.setSessionStart now nextActive.session }
        , True
        )

    else
        ( nextActive, False )


sessionChangeToFlash : Session.SessionDef -> Session.SessionDef -> ( Flash.FlashMsg, String )
sessionChangeToFlash from to =
    case Session.sessionChangeToLabel from to of
        "" ->
            ( Flash.empty, "" )

        label ->
            ( Flash.new label, label )


evalElapsedTime : Model a -> EvalResult msg
evalElapsedTime { active, sessions, settings, time } =
    if Session.secondsLeft active == 0 then
        let
            nextIndex =
                active.index + 1

            ( newActive, playing ) =
                rollActiveSession time nextIndex settings.flow sessions

            ( flashMsg, notificationMsg ) =
                sessionChangeToFlash active.session.def newActive.session.def

            sentimentSession =
                if active.session.def |> Session.isWork then
                    Just active.session

                else
                    Nothing

            notificationCmd =
                { sound = Settings.alarmSoundToEncodable settings.alarmSound
                , msg = notificationMsg
                , config = settings.notifications
                }
                    |> encodeNotificationConfig
                    |> Ports.notify

            spotifyCmd =
                if Session.isWork newActive.session.def then
                    Spotify.play settings.spotify

                else
                    Spotify.pause settings.spotify

            logCmd =
                Session.logSession time active
        in
        EvalResult
            newActive
            playing
            (Just flashMsg)
            (Cmd.batch [ notificationCmd, spotifyCmd, logCmd ])
            sentimentSession

    else
        EvalResult (Session.addElapsed 1 active) True Nothing Cmd.none Nothing


updateTime : Time.Posix -> Model a -> Model a
updateTime now model =
    { model | time = now, uptime = model.uptime + 1 }


setupSentimentSession :
    Maybe Session.Session
    -> Session.SessionDef
    -> Model a
    -> Model a
setupSentimentSession session sessionDef model =
    let
        newSession =
            case ( model.sentimentSession, session, Session.isWork sessionDef ) of
                ( _, _, True ) ->
                    Nothing

                ( sentiment, Nothing, _ ) ->
                    sentiment

                ( Nothing, sentiment, _ ) ->
                    sentiment

                _ ->
                    Nothing
    in
    { model | sentimentSession = newSession }


tick : Time.Posix -> Model a -> ( Model a, Cmd msg )
tick posix ({ playing, flash, active, settings } as model) =
    if playing then
        let
            newState =
                evalElapsedTime model

            setFlashFn =
                if settings.notifications.inApp then
                    Flash.setFlash newState.flash

                else
                    identity
        in
        { model
            | active = newState.active
            , playing = newState.playing
            , flash = flash |> Maybe.andThen Flash.updateFlashTime
        }
            |> setupSentimentSession newState.sentimentSession newState.active.session.def
            |> updateTime posix
            |> setFlashFn
            |> Misc.withCmd
            |> Misc.addCmd newState.cmd
            |> Misc.addCmd (Session.saveActive newState.active)

    else
        { model | flash = flash |> Maybe.andThen Flash.updateFlashTime }
            |> updateTime posix
            |> Misc.withCmd



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



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.tick Tick



-- CODECS


encodeNotificationConfig : { sound : String, msg : String, config : Settings.Notifications } -> Encode.Value
encodeNotificationConfig { sound, msg, config } =
    Encode.object
        [ ( "sound", Encode.string sound )
        , ( "msg", Encode.string msg )
        , ( "config", Settings.encodeNotifications config )
        ]
