module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Color
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Iso8601
import Json.Decode as Decode
import List.Extra as ListEx
import Misc
import Page.Flash as Flash
import Page.Settings as Settings
import Page.Stats as Stats exposing (StatState)
import Page.Timer as Timer
import Platform exposing (Program)
import Platform.Sub as Sub
import Session
import Task
import Theme.Common exposing (Theme)
import Theme.Theme as Theme
import Time exposing (Posix, Zone)
import Url exposing (Url)
import VirtualDom exposing (Node)



-- MODEL


type alias Model =
    { zone : Zone
    , time : Posix
    , key : Key
    , page : Page
    , uptime : Int
    , playing : Bool
    , settings : Settings.Settings
    , sessions : List Session.SessionDef
    , active : Session.Active
    , sentimentSession : Maybe Session.Session
    , flash : Maybe (Flash.FlashMsg Msg)
    }


type Page
    = TimerPage
    | StatsPage StatState
    | SettingsPage
    | CreditsPage


type alias Flags =
    { active : Decode.Value
    , settings : Decode.Value
    , now : Int
    }



-- INIT


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
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


view : Model -> Document Msg
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


viewBody : Model -> Node Msg
viewBody model =
    Html.div
        [ HtmlAttr.class "container"
        , HtmlAttr.css
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


viewFlash : Theme -> Maybe (FlashMsg Msg) -> Html Msg
viewFlash theme flash =
    flash
        |> Maybe.map (\f -> Flash.render theme f)
        |> Maybe.withDefault (Html.text "")


viewNav : Theme -> Page -> Html Msg
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
                , Css.color <| (theme |> Theme.backgroundColor |> Colors.toCssColor)
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
        [ HtmlAttr.css
            [ Css.position Css.absolute
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.backgroundColor <| (theme |> Theme.foregroundColor |> Colors.toCssColor)
            , Css.color <| (theme |> Theme.foregroundColor |> Colors.toCssColor)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.padding <| Css.rem 0.25
            ]
        ]
        [ Html.ul
            [ HtmlAttr.css
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
                                [ HtmlAttr.href path
                                , HtmlAttr.css
                                    [ buttonStyle
                                    , isSelected path page
                                    ]
                                ]
                                [ Common.icon icon ]
                            ]
                    )
            )
        ]


viewPage : Model -> Html Msg
viewPage model =
    Html.div
        [ HtmlAttr.css
            [ Css.height (Css.calc (Css.pct 100) Css.minus (Css.rem 3.5))
            , Css.overflow Css.auto
            ]
        ]
        [ case model.page of
            TimerPage ->
                Timer.view model |> Html.map Timer

            SettingsPage ->
                Settings.view model |> Html.map Settings

            StatsPage _ ->
                Stats.view model |> Html.map Stats

            CreditsPage ->
                Credits.view model
        ]



-- UPDATE


type Msg
    = AdjustTimeZone Zone
    | UrlChanged Url
    | LinkCliked UrlRequest
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
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External href ->
                    ( model, Nav.load href )

        ( Timer subMsg, TimerPage ) ->
            Timer.update subMsg model

        ( Stats subMsg, StatsPage state ) ->
            Misc.withCmd model

        ( Settings subMsg, SettingsPage ) ->
            Settings.update subMsg model |> Misc.updateWith Settings

        ChangeLogDate newDate ->
            case model.page of
                StatsPage (Loaded def) ->
                    done { model | page = StatsPage (Loaded { def | date = newDate }) }

                _ ->
                    done model

        ChangeLogMonth newDate ->
            case newDate |> Date.add Date.Days 1 |> Date.toIsoString |> Iso8601.toTime of
                Ok posix ->
                    ( model, fetchLogs <| Time.posixToMillis posix )

                Err _ ->
                    done model

        GotStatsLogs raw ->
            case ( model.page, D.decodeValue Decoder.decodeLog raw ) of
                ( StatsPage Loading, Ok { ts, logs } ) ->
                    let
                        date =
                            ts |> Time.millisToPosix |> Date.fromPosix model.zone
                    in
                    done { model | page = StatsPage (Loaded (StatsDef date logs False)) }

                ( StatsPage (Loaded def), Ok { ts, logs } ) ->
                    let
                        newDef =
                            { def
                                | date =
                                    ts
                                        |> Time.millisToPosix
                                        |> Date.fromPosix model.zone
                                , logs = logs
                            }
                    in
                    done { model | page = StatsPage (Loaded newDef) }

                _ ->
                    done model

        CloseFlashMsg ->
            { model | flash = Nothing } |> done

        GotFlashMsg raw ->
            case D.decodeValue decodeFlash raw of
                Ok flash ->
                    { model | flash = Just flash } |> done

                Err _ ->
                    done model

        UpdateSentiment start sentiment ->
            let
                newModel =
                    case model.page of
                        StatsPage (Loaded def) ->
                            let
                                newLogs =
                                    def.logs
                                        |> ListEx.findIndex (.start >> (==) (Just start))
                                        |> Maybe.map
                                            (\idx ->
                                                def.logs
                                                    |> ListEx.updateAt idx
                                                        (\cycle -> { cycle | sentiment = Just sentiment })
                                            )
                                        |> Maybe.withDefault def.logs
                            in
                            { model
                                | page = StatsPage (Loaded { def | logs = newLogs })
                                , sentimentCycle = Nothing
                            }

                        _ ->
                            { model | sentimentCycle = Nothing }
            in
            ( newModel, updateCycle ( Time.posixToMillis start, Tools.sentimentToString sentiment ) )

        ToggleLogs ->
            case model.page of
                StatsPage (Loaded def) ->
                    let
                        newDef =
                            { def | showLogs = not def.showLogs }
                    in
                    done { model | page = StatsPage (Loaded newDef) }

                _ ->
                    done model

        ClearLogs ->
            ( model, clearLogs () )



-- HELPERS


default : Key -> Model
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


urlToPage : Time.Posix -> Url -> ( Page, Cmd Msg )
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
    --     , gotFlashMsg GotFlashMsg
    Sub.batch
        [ Timer.subscriptions |> Sub.map Timer
        , Settings.subscriptions |> Sub.map Settings
        , Stats.subscriptions |> Sub.map Stats
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



-- decodeFlash : D.Decoder (FlashMsg msg)
-- decodeFlash =
--     D.map2
--         (\title msg -> newFlash title (Html.div [] [ Html.text msg ]))
--         (D.field "title" D.string)
--         (D.field "msg" D.string)
