module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Color
import Css
import Elements
import Env
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Misc
import Page.Credits as Credits
import Page.Flash as Flash
import Page.Global as Global
import Page.Preferences as Preferences
import Page.Stats as Stats
import Page.Timer as Timer
import Platform.Sub as Sub
import Sessions
import Settings
import Task
import Theme.Common
import Theme.Theme as Theme
import Time
import Url
import VirtualDom



-- MODEL


type Model
    = Timer Timer.Model
    | Stats Stats.Model
    | Preferences Preferences.Model
    | Credits Credits.Model


type alias Flags =
    { active : Decode.Value
    , settings : Decode.Value
    , now : Int
    }



-- INIT


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { active, settings, now } url key =
    let
        baseModel =
            default key |> mapEnv (\e -> { e | time = Time.millisToPosix now })

        sessions =
            getSessions baseModel

        newActive =
            case Decode.decodeValue Sessions.decodeActive active of
                Ok active_ ->
                    active_

                Err _ ->
                    sessions.active

        newSettings =
            case Decode.decodeValue Settings.decodeSettings settings of
                Ok settings_ ->
                    settings_

                Err _ ->
                    getSettings baseModel

        ( newSessions, newActive_ ) =
            Sessions.buildSessions newSettings (Just newActive)

        newSessions_ =
            { sessions | active = newActive_, sessions = newSessions }

        ( model, pageCmd ) =
            urlToPage url baseModel
    in
    ( model
        |> mapEnv (\e -> { e | time = Time.millisToPosix now })
        |> mapSettings (always newSettings)
        |> mapSessions (always newSessions_)
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , pageCmd
        ]
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        sessions =
            getSessions model

        title =
            case model of
                Timer _ ->
                    if sessions.playing then
                        [ sessions.active |> Sessions.secondsLeft |> truncate |> Sessions.secondsToDisplay
                        , Sessions.sessionDefToString sessions.active.session.def
                        ]

                    else
                        []

                Preferences _ ->
                    [ "Preferences" ]

                Stats _ ->
                    [ "Stats" ]

                Credits _ ->
                    [ "Credits" ]
    in
    { title = title ++ [ "Pelmodoro" ] |> String.join " - "
    , body = [ viewBody model ]
    }


viewBody : Model -> VirtualDom.Node Msg
viewBody model =
    let
        settings =
            getSettings model
    in
    Html.div
        [ Attributes.class "container"
        , Attributes.css
            [ Css.width <| Css.vw 100.0
            , Css.position Css.relative
            , Css.backgroundColor <| (settings.theme |> Theme.backgroundColor |> Color.toCssColor)
            , Css.fontFamilies [ "Montserrat" ]
            , Css.color (settings.theme |> Theme.textColor |> Color.toCssColor)
            ]
        ]
        [ viewPage model
        , viewFlash settings.theme <| getFlash model
        , viewNav model
        ]
        |> Html.toUnstyled


viewFlash : Theme.Common.Theme -> Maybe (Flash.FlashMsg Flash.Msg) -> Html.Html Msg
viewFlash theme flash =
    flash
        |> Maybe.map (\f -> Flash.view theme f |> Html.map FlashMsg)
        |> Maybe.withDefault (Html.text "")


viewNav : Model -> Html.Html Msg
viewNav model =
    let
        pages =
            [ ( "/", "timer" )
            , ( "/stats", "leaderboard" )
            , ( "/preferences", "settings" )
            , ( "/credits", "info" )
            ]

        settings =
            getSettings model

        theme =
            settings.theme

        buttonStyle =
            Css.batch
                [ Css.borderStyle Css.none
                , Css.backgroundColor Css.transparent
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.color <| (theme |> Theme.backgroundColor |> Color.toCssColor)
                , Css.outline Css.zero
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.textDecoration Css.none
                ]

        isSelected path =
            case ( path, model ) of
                ( "/", Timer _ ) ->
                    Css.opacity <| Css.num 1

                ( "/settings", Preferences _ ) ->
                    Css.opacity <| Css.num 1

                ( "/preferences", Preferences _ ) ->
                    Css.opacity <| Css.num 1

                ( "/stats", Stats _ ) ->
                    Css.opacity <| Css.num 1

                ( "/credits", Credits _ ) ->
                    Css.opacity <| Css.num 1

                _ ->
                    Css.opacity <| Css.num 0.4
    in
    Html.div
        [ Attributes.css
            [ Css.position Css.absolute
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.backgroundColor <| (theme |> Theme.foregroundColor |> Color.toCssColor)
            , Css.color <| (theme |> Theme.foregroundColor |> Color.toCssColor)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.padding <| Css.rem 0.25
            ]
        ]
        [ Html.ul
            [ Attributes.css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.listStyle Css.none
                ]
            ]
            (pages
                |> List.map
                    (\( path, icon ) ->
                        Html.li []
                            [ Html.a
                                [ Attributes.href path
                                , Attributes.css
                                    [ buttonStyle
                                    , isSelected path
                                    ]
                                ]
                                [ Elements.icon icon ]
                            ]
                    )
            )
        ]


viewPage : Model -> Html.Html Msg
viewPage model =
    Html.div
        [ Attributes.css
            [ Css.height (Css.calc (Css.pct 100) Css.minus (Css.rem 3.5))
            , Css.overflow Css.auto
            ]
        ]
        [ case model of
            Timer m ->
                Timer.view m |> Html.map TimerMsg

            Preferences m ->
                Preferences.view m |> Html.map PreferencesMsg

            Stats m ->
                Stats.view m |> Html.map StatsMsg

            Credits m ->
                Credits.view m
        ]



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | UrlChanged Url.Url
    | LinkCliked Browser.UrlRequest
    | GlobalMsg Global.Msg
    | TimerMsg Timer.Msg
    | StatsMsg Stats.Msg
    | PreferencesMsg Preferences.Msg
    | FlashMsg Flash.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AdjustTimeZone newZone, _ ) ->
            model
                |> mapEnv (\e -> { e | zone = newZone })
                |> Misc.withCmd

        ( UrlChanged url, _ ) ->
            model |> urlToPage url

        ( LinkCliked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        env =
                            getEnv model
                    in
                    ( model, Navigation.pushUrl env.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( GlobalMsg _, _ ) ->
            Misc.withCmd model

        ( TimerMsg subMsg, Timer m ) ->
            Timer.update subMsg m |> Misc.updateWith TimerMsg |> Tuple.mapFirst Timer

        ( StatsMsg subMsg, Stats m ) ->
            Stats.update subMsg m |> Misc.updateWith StatsMsg |> Tuple.mapFirst Stats

        ( PreferencesMsg subMsg, Preferences m ) ->
            Preferences.update subMsg m |> Misc.updateWith PreferencesMsg |> Tuple.mapFirst Preferences

        _ ->
            Misc.withCmd model



-- HELPERS


default : Navigation.Key -> Model
default key =
    let
        ( sessions, active ) =
            Sessions.buildSessions Settings.default Nothing

        sessions_ =
            Sessions.Sessions active False 0 sessions

        env =
            Env.Env Time.utc (Time.millisToPosix 0) key
    in
    Timer <| Timer.new env Settings.default sessions_ Nothing


urlToPage : Url.Url -> Model -> ( Model, Cmd Msg )
urlToPage { path } model =
    let
        env =
            getEnv model

        settings =
            getSettings model

        sessions =
            getSessions model

        flash =
            getFlash model
    in
    case path of
        "/settings" ->
            ( Preferences <| Preferences.new env settings sessions flash, Cmd.none )

        "/preferences" ->
            ( Preferences <| Preferences.new env settings sessions flash, Cmd.none )

        "/stats" ->
            ( Stats <| Stats.new env settings sessions flash, Stats.logsFetchCmd env.time )

        "/credits" ->
            ( Credits <| Credits.new env settings sessions flash, Cmd.none )

        _ ->
            ( Timer <| Timer.new env settings sessions flash, Cmd.none )


getFlash : Model -> Flash.Flash
getFlash model =
    case model of
        Timer { flash } ->
            flash

        Preferences { flash } ->
            flash

        Stats { flash } ->
            flash

        Credits { flash } ->
            flash


getSessions : Model -> Sessions.Sessions
getSessions model =
    case model of
        Timer { sessions } ->
            sessions

        Preferences { sessions } ->
            sessions

        Stats { sessions } ->
            sessions

        Credits { sessions } ->
            sessions


getSettings : Model -> Settings.Settings
getSettings model =
    case model of
        Timer { settings } ->
            settings

        Preferences { settings } ->
            settings

        Stats { settings } ->
            settings

        Credits { settings } ->
            settings


getEnv : Model -> Env.Env
getEnv model =
    case model of
        Timer { env } ->
            env

        Preferences { env } ->
            env

        Stats { env } ->
            env

        Credits { env } ->
            env


mapSettings : (Settings.Settings -> Settings.Settings) -> Model -> Model
mapSettings fn model =
    case model of
        Timer m ->
            Timer { m | settings = fn m.settings }

        Preferences m ->
            Preferences { m | settings = fn m.settings }

        Stats m ->
            Stats { m | settings = fn m.settings }

        Credits m ->
            Credits { m | settings = fn m.settings }


mapSessions : (Sessions.Sessions -> Sessions.Sessions) -> Model -> Model
mapSessions fn model =
    case model of
        Timer m ->
            Timer { m | sessions = fn m.sessions }

        Preferences m ->
            Preferences { m | sessions = fn m.sessions }

        Stats m ->
            Stats { m | sessions = fn m.sessions }

        Credits m ->
            Credits { m | sessions = fn m.sessions }


mapEnv : (Env.Env -> Env.Env) -> Model -> Model
mapEnv fn model =
    case model of
        Timer m ->
            Timer { m | env = fn m.env }

        Preferences m ->
            Preferences { m | env = fn m.env }

        Stats m ->
            Stats { m | env = fn m.env }

        Credits m ->
            Credits { m | env = fn m.env }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Timer.subscriptions |> Sub.map TimerMsg
        , Preferences.subscriptions |> Sub.map PreferencesMsg
        , Stats.subscriptions |> Sub.map StatsMsg
        , Flash.subscriptions |> Sub.map FlashMsg
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkCliked
        }
