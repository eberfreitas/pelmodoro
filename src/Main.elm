module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Color
import Css
import Elements
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode
import Misc
import Page.Credits as Credits
import Page.Flash as Flash
import Page.Settings as Settings
import Page.Stats as Stats
import Page.Timer as Timer
import Platform.Sub as Sub
import Session
import Task
import Theme.Common
import Theme.Theme as Theme
import Time
import Url
import VirtualDom



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , key : Navigation.Key
    , page : Page
    , uptime : Int
    , playing : Bool
    , settings : Settings.Settings
    , sessions : List Session.SessionDef
    , active : Session.Active
    , sentimentSession : Maybe Session.Session
    , flash : Maybe (Flash.FlashMsg Flash.Msg)
    }


type Page
    = TimerPage
    | StatsPage Stats.State
    | SettingsPage
    | CreditsPage


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
            default key

        newActive =
            case Decode.decodeValue Session.decodeActive active of
                Ok active_ ->
                    active_

                Err _ ->
                    baseModel.active

        newSettings =
            case Decode.decodeValue Settings.decodeSettings settings of
                Ok settings_ ->
                    settings_

                Err _ ->
                    baseModel.settings

        time =
            Time.millisToPosix now

        ( newSessions, newActive_ ) =
            Session.buildSessions newSettings (Just newActive)

        ( page, pageCmd ) =
            urlToPage time url
    in
    ( { baseModel
        | active = newActive_
        , time = Time.millisToPosix now
        , settings = newSettings
        , sessions = newSessions
        , page = page
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , pageCmd
        ]
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            case model.page of
                TimerPage ->
                    if model.playing then
                        [ model.active |> Session.secondsLeft |> truncate |> Timer.secondsToDisplay
                        , Session.sessionDefToString model.active.session.def
                        ]

                    else
                        []

                SettingsPage ->
                    [ "Settings" ]

                StatsPage _ ->
                    [ "Stats" ]

                CreditsPage ->
                    [ "Credits" ]
    in
    { title = title ++ [ "Pelmodoro" ] |> String.join " - "
    , body = [ viewBody model ]
    }


viewBody : Model -> VirtualDom.Node Msg
viewBody model =
    Html.div
        [ Attributes.class "container"
        , Attributes.css
            [ Css.width <| Css.vw 100.0
            , Css.position Css.relative
            , Css.backgroundColor <| (model.settings.theme |> Theme.backgroundColor |> Color.toCssColor)
            , Css.fontFamilies [ "Montserrat" ]
            , Css.color (model.settings.theme |> Theme.textColor |> Color.toCssColor)
            ]
        ]
        [ viewPage model
        , viewFlash model.settings.theme model.flash
        , viewNav model.settings.theme model.page
        ]
        |> Html.toUnstyled


viewFlash : Theme.Common.Theme -> Maybe (Flash.FlashMsg Flash.Msg) -> Html.Html Msg
viewFlash theme flash =
    flash
        |> Maybe.map (\f -> Flash.view theme f |> Html.map Flash)
        |> Maybe.withDefault (Html.text "")


viewNav : Theme.Common.Theme -> Page -> Html.Html Msg
viewNav theme page =
    let
        pages =
            [ ( "/", "timer" )
            , ( "/stats", "leaderboard" )
            , ( "/settings", "settings" )
            , ( "/credits", "info" )
            ]

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

        isSelected path current =
            case ( path, current ) of
                ( "/", TimerPage ) ->
                    Css.opacity <| Css.num 1

                ( "/settings", SettingsPage ) ->
                    Css.opacity <| Css.num 1

                ( "/stats", StatsPage _ ) ->
                    Css.opacity <| Css.num 1

                ( "/credits", CreditsPage ) ->
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
                                    , isSelected path page
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
        [ case model.page of
            TimerPage ->
                Timer.view model |> Html.map Timer

            SettingsPage ->
                Settings.view model |> Html.map Settings

            StatsPage state ->
                Stats.view model state |> Html.map Stats

            CreditsPage ->
                Credits.view model
        ]



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | UrlChanged Url.Url
    | LinkCliked Browser.UrlRequest
    | Timer Timer.Msg
    | Stats Stats.Msg
    | Settings Settings.Msg
    | Flash Flash.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( AdjustTimeZone newZone, _ ) ->
            Misc.withCmd { model | zone = newZone }

        ( UrlChanged url, _ ) ->
            url
                |> urlToPage model.time
                |> Tuple.mapFirst (\p -> { model | page = p })

        ( LinkCliked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( Flash subMsg, _ ) ->
            Flash.update subMsg model |> Misc.updateWith Flash

        ( Timer subMsg, _ ) ->
            Timer.update subMsg model

        ( Stats subMsg, StatsPage state ) ->
            Stats.update model.zone subMsg state
                |> Tuple.mapFirst (\s -> { model | page = StatsPage s })
                |> Misc.updateWith Stats

        ( Settings subMsg, _ ) ->
            Settings.update subMsg model |> Misc.updateWith Settings

        _ ->
            Misc.withCmd model



-- HELPERS


default : Navigation.Key -> Model
default key =
    let
        ( sessions, active ) =
            Session.buildSessions Settings.default Nothing
    in
    { zone = Time.utc
    , time = Time.millisToPosix 0
    , key = key
    , page = TimerPage
    , uptime = 0
    , playing = False
    , settings = Settings.default
    , sessions = sessions
    , active = active
    , sentimentSession = Nothing
    , flash = Nothing
    }


urlToPage : Time.Posix -> Url.Url -> ( Page, Cmd Msg )
urlToPage time { path } =
    case path of
        "/settings" ->
            ( SettingsPage, Cmd.none )

        "/stats" ->
            ( StatsPage Stats.initialState, time |> Stats.logsFetchCmd )

        "/credits" ->
            ( CreditsPage, Cmd.none )

        _ ->
            ( TimerPage, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Timer.subscriptions |> Sub.map Timer
        , Settings.subscriptions |> Sub.map Settings
        , Stats.subscriptions |> Sub.map Stats
        , Flash.subscriptions |> Sub.map Flash
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
