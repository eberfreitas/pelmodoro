module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Color
import Component.Flash
import Component.Tick
import Css
import Elements
import Env
import Flash
import Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Misc
import Page.Credits as Credits
import Page.Preferences as Preferences
import Page.Stats as Stats
import Page.Timer as Timer
import Platform.Sub as Sub
import Ports
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


viewFlash : Theme.Common.Theme -> Flash.Flash -> Html.Html Msg
viewFlash theme flash =
    Component.Flash.view theme flash |> Html.map FlashMsg


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
    | Tick Decode.Value
    | TimerMsg Timer.Msg
    | StatsMsg Stats.Msg
    | PreferencesMsg Preferences.Msg
    | FlashMsg Component.Flash.Msg



-- | FlashMsg Flash.Msg


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

        ( Tick raw, _ ) ->
            case Decode.decodeValue Decode.int raw of
                Ok millis ->
                    let
                        ( newGlobal, cmd ) =
                            model |> getGlobal |> Component.Tick.tick (Time.millisToPosix millis)
                    in
                    ( model |> mapGlobal (always newGlobal), cmd )

                Err _ ->
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

        global =
            Global.Global env Settings.default sessions_ Nothing Nothing
    in
    Timer <| Timer.new global


urlToPage : Url.Url -> Model -> ( Model, Cmd Msg )
urlToPage { path } model =
    let
        global =
            getGlobal model
    in
    case path of
        "/settings" ->
            ( Preferences <| Preferences.new global, Cmd.none )

        "/preferences" ->
            ( Preferences <| Preferences.new global, Cmd.none )

        "/stats" ->
            ( Stats <| Stats.new global, Stats.logsFetchCmd global.env.time )

        "/credits" ->
            ( Credits <| Credits.new global, Cmd.none )

        _ ->
            ( Timer <| Timer.new global, Cmd.none )


getGlobal : Model -> Global.Global
getGlobal model =
    case model of
        Timer { global } ->
            global

        Preferences { global } ->
            global

        Stats { global } ->
            global

        Credits { global } ->
            global


mapGlobal : (Global.Global -> Global.Global) -> Model -> Model
mapGlobal mapFn model =
    case model of
        Timer m ->
            Timer { m | global = mapFn m.global }

        Preferences m ->
            Preferences { m | global = mapFn m.global }

        Stats m ->
            Stats { m | global = mapFn m.global }

        Credits m ->
            Credits { m | global = mapFn m.global }


getFlash : Model -> Flash.Flash
getFlash model =
    model |> getGlobal |> .flash


getSessions : Model -> Sessions.Sessions
getSessions model =
    model |> getGlobal |> .sessions


mapSessions : (Sessions.Sessions -> Sessions.Sessions) -> Model -> Model
mapSessions mapFn =
    mapGlobal (\s -> { s | sessions = mapFn s.sessions })


getSettings : Model -> Settings.Settings
getSettings model =
    model |> getGlobal |> .settings


mapSettings : (Settings.Settings -> Settings.Settings) -> Model -> Model
mapSettings mapFn =
    mapGlobal (\s -> { s | settings = mapFn s.settings })


getEnv : Model -> Env.Env
getEnv model =
    model |> getGlobal |> .env


mapEnv : (Env.Env -> Env.Env) -> Model -> Model
mapEnv mapFn =
    mapGlobal (\g -> { g | env = mapFn g.env })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Preferences.subscriptions |> Sub.map PreferencesMsg
        , Stats.subscriptions |> Sub.map StatsMsg
        , Component.Flash.subscriptions |> Sub.map FlashMsg
        , Ports.tick Tick
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
