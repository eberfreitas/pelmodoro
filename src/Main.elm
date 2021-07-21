port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Css
import File
import File.Select as Select
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Iso8601
import Json.Decode as D
import List.Extra as ListEx
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
    | SettingsPage
    | StatsPage StatState
    | CreditsPage


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


port fetchLogs : Int -> Cmd msg


port requestBrowserNotif : Bool -> Cmd msg


port requestDataExport : () -> Cmd msg


port importData : String -> Cmd msg


port updateCycle : ( Int, String ) -> Cmd msg


port testSound : String -> Cmd msg


port clearLogs : () -> Cmd msg


port gotSpotifyState : (D.Value -> msg) -> Sub msg


port gotStatsLogs : (D.Value -> msg) -> Sub msg


port gotFlashMsg : (D.Value -> msg) -> Sub msg


port gotBrowserNotifRes : (D.Value -> msg) -> Sub msg


type alias Flags =
    { current : D.Value
    , settings : D.Value
    , now : Int
    }


urlToPage : Int -> Url -> ( Page, Cmd Msg )
urlToPage time { path } =
    case path of
        "/settings" ->
            ( SettingsPage, Cmd.none )

        "/stats" ->
            ( StatsPage Loading, fetchLogs time )

        "/credits" ->
            ( CreditsPage, Cmd.none )

        _ ->
            ( TimerPage, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { current, settings, now } url key =
    let
        baseModel =
            Model.default key

        newCurrent =
            case D.decodeValue Decoder.decodeCurrent current of
                Ok curr ->
                    curr

                Err _ ->
                    baseModel.current

        newSettings =
            case D.decodeValue Decoder.decodeSettings settings of
                Ok settings_ ->
                    settings_

                Err _ ->
                    baseModel.settings

        ( newIntervals, newCurrent_ ) =
            Model.buildIntervals newSettings (Just newCurrent)

        ( page, pageCmd ) =
            urlToPage now url
    in
    ( { baseModel
        | current = newCurrent_
        , time = Time.millisToPosix now
        , settings = newSettings
        , intervals = newIntervals
        , page = page
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , pageCmd
        ]
    )


view : Model -> Document Msg
view model =
    let
        title =
            case model.page of
                TimerPage ->
                    if model.playing then
                        [ model.current |> Model.currentSecondsLeft |> truncate |> Timer.secondsToDisplay
                        , Model.intervalToString model.current.cycle.interval
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
            , Css.backgroundColor <| (model.settings.theme |> Theme.backgroundColor |> Colors.toCssColor)
            , Css.fontFamilies [ "Montserrat" ]
            , Css.color (model.settings.theme |> Theme.textColor |> Colors.toCssColor)
            ]
        ]
        [ renderPage model
        , renderFlash model.settings.theme model.flash
        , renderNav model.settings.theme model.page
        ]
        |> Html.toUnstyled


renderFlash : Theme -> Maybe (FlashMsg Msg) -> Html Msg
renderFlash theme flash =
    flash
        |> Maybe.map (\f -> Flash.render theme f)
        |> Maybe.withDefault (Html.text "")


renderNav : Theme -> Page -> Html Msg
renderNav theme page =
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


renderPage : Model -> Html Msg
renderPage model =
    Html.div
        [ HtmlAttr.css
            [ Css.height (Css.calc (Css.pct 100) Css.minus (Css.rem 3.5))
            , Css.overflow Css.auto
            ]
        ]
        [ case model.page of
            TimerPage ->
                Timer.render model

            SettingsPage ->
                Settings.render model

            StatsPage _ ->
                Stats.render model

            CreditsPage ->
                Credits.render model
        ]


handleFlashMsg : Maybe (FlashMsg msg) -> Maybe (FlashMsg msg)
handleFlashMsg flashMsg =
    flashMsg
        |> Maybe.andThen
            (\({ time } as flash) ->
                if (time - 1) < 1 then
                    Nothing

                else
                    Just { flash | time = time - 1 }
            )


decodeFlash : D.Decoder (FlashMsg msg)
decodeFlash =
    D.map2
        (\title msg -> newFlash title (Html.div [] [ Html.text msg ]))
        (D.field "title" D.string)
        (D.field "msg" D.string)


decodeBrowserNotifRes : D.Decoder { val : Bool, msg : String }
decodeBrowserNotifRes =
    D.map2
        (\val msg -> { val = val, msg = msg })
        (D.field "val" D.bool)
        (D.field "msg" D.string)


type Msg
    = NoOp
    | AdjustTimeZone Zone
    | UrlChanged Url
    | LinkCliked UrlRequest
    | Timer Timer.Msg
    | Stats Stats.Msg
    | Settings Settings.Msg
    | Flash Flash.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        done m =
            ( m, Cmd.none )

        playPlaylist uri ( model_, cmd ) =
            spotifyPlay uri
                |> Helpers.flip (::) [ cmd ]
                |> Cmd.batch
                |> Tuple.pair model_

        pausePlaylist ( model_, cmd ) =
            spotifyPause ()
                |> Helpers.flip (::) [ cmd ]
                |> Cmd.batch
                |> Tuple.pair model_

        persistCurrent_ ( model_, cmd ) =
            model_.current
                |> Encoder.encodeCurrent
                |> persistCurrent
                |> Helpers.flip (::) [ cmd ]
                |> Cmd.batch
                |> Tuple.pair model_

        persistSettings_ ( model_, cmd ) =
            model_.settings
                |> Encoder.encodeSettings
                |> persistSettings
                |> Helpers.flip (::) [ cmd ]
                |> Cmd.batch
                |> Tuple.pair model_

        updateFlash ( model_, cmd ) =
            { model_ | flash = handleFlashMsg model_.flash }
                |> Helpers.flip Tuple.pair cmd

        updateSettings model_ =
            let
                ( newIntervals, newCurrent ) =
                    Model.buildIntervals model_.settings (Just model_.current)
            in
            { model_ | current = newCurrent, intervals = newIntervals, playing = False }
                |> done
                |> persistSettings_
                |> persistCurrent_
                |> pausePlaylist
    in
    case msg of
        NoOp ->
            done model

        Timer subMsg ->
            Timer.update subMsg model

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        ChangeRounds rounds ->
            model
                |> Model.mapSettings (\s -> { s | rounds = rounds })
                |> updateSettings

        ChangeActivity mins ->
            model
                |> Model.mapSettings (\s -> { s | activity = mins * 60 })
                |> updateSettings

        ChangeBreak mins ->
            model
                |> Model.mapSettings (\s -> { s | break = mins * 60 })
                |> updateSettings

        ChangeLongBreak mins ->
            model
                |> Model.mapSettings (\s -> { s | longBreak = mins * 60 })
                |> updateSettings

        ChangeContinuity cont ->
            case Tools.continuityFromDisplay cont of
                Just c ->
                    model
                        |> Model.mapSettings (\s -> { s | continuity = c })
                        |> updateSettings

                Nothing ->
                    done model

        ChangeTheme theme ->
            case Themes.Types.themeFromString theme of
                Just t ->
                    model
                        |> Model.mapSettings (\s -> { s | theme = t })
                        |> updateSettings

                Nothing ->
                    done model

        ChangePlaylist uri ->
            model
                |> Model.mapSettings
                    (\s ->
                        let
                            newSpotify =
                                case s.spotify of
                                    Connected playlists _ ->
                                        Connected
                                            playlists
                                            (ListEx.find
                                                (Tuple.first >> (==) uri)
                                                playlists
                                                |> Maybe.map Tuple.first
                                            )

                                    _ ->
                                        s.spotify
                        in
                        { s | spotify = newSpotify }
                    )
                |> updateSettings

        GotSpotifyState raw ->
            case D.decodeValue Decoder.decodeSpotify raw of
                Ok newState ->
                    model
                        |> Model.mapSettings
                            (\s ->
                                let
                                    newSpotify =
                                        case ( s.spotify, newState ) of
                                            ( Connected _ (Just current), Connected playlists _ ) ->
                                                let
                                                    newCurrent =
                                                        playlists
                                                            |> ListEx.find (Tuple.first >> (==) current)
                                                            |> Maybe.map Tuple.first
                                                in
                                                Connected playlists newCurrent

                                            _ ->
                                                newState
                                in
                                { s | spotify = newSpotify }
                            )
                        |> updateSettings

                Err _ ->
                    done model

        SpotifyRefresh ->
            ( model, spotifyRefresh () )

        SpotifyDisconnect ->
            ( model, spotifyDisconnect () )

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

        UrlChanged url ->
            url
                |> urlToPage (Time.posixToMillis model.time)
                |> Tuple.mapFirst (\p -> { model | page = p })

        LinkCliked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External href ->
                    ( model, Nav.load href )

        CloseFlashMsg ->
            { model | flash = Nothing } |> done

        GotFlashMsg raw ->
            case D.decodeValue decodeFlash raw of
                Ok flash ->
                    { model | flash = Just flash } |> done

                Err _ ->
                    done model

        ToggleNotification type_ ->
            let
                notificationSettings =
                    model.settings.notifications
            in
            case type_ of
                InApp ->
                    let
                        newNotifications =
                            { notificationSettings | inApp = not notificationSettings.inApp }
                    in
                    model
                        |> Model.mapSettings (\s -> { s | notifications = newNotifications })
                        |> updateSettings

                Sound ->
                    let
                        newNotifications =
                            { notificationSettings | sound = not notificationSettings.sound }
                    in
                    model
                        |> Model.mapSettings (\s -> { s | notifications = newNotifications })
                        |> updateSettings

                Browser ->
                    ( model, requestBrowserNotif (not model.settings.notifications.browser) )

        GotBrowserNotifRes raw ->
            case D.decodeValue decodeBrowserNotifRes raw of
                Ok res ->
                    let
                        flashFn =
                            if res.msg /= "" then
                                newFlash "Attention" (Html.div [] [ Html.text res.msg ])
                                    |> Just
                                    |> setFlash

                            else
                                identity

                        notifications =
                            model.settings.notifications

                        newNotifications =
                            { notifications | browser = res.val }
                    in
                    model
                        |> Model.mapSettings (\s -> { s | notifications = newNotifications })
                        |> updateSettings
                        |> flashFn

                Err _ ->
                    done model

        RequestDataExport ->
            ( model, requestDataExport () )

        ImportRequest ->
            ( model, Select.file [ "application/json" ] ImportSelect )

        ImportSelect file ->
            ( model, Task.perform ImportData (File.toString file) )

        ImportData data ->
            ( model, importData data )

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

        ChangeSound sound ->
            case Tools.soundFromDisplay sound of
                Just s ->
                    model
                        |> Model.mapSettings (\se -> { se | sound = s })
                        |> updateSettings

                Nothing ->
                    done model

        TestSound sound ->
            ( model, testSound (sound |> Tools.soundToString) )

        ClearLogs ->
            ( model, clearLogs () )


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ tick Tick
        , gotSpotifyState GotSpotifyState
        , gotStatsLogs GotStatsLogs
        , gotFlashMsg GotFlashMsg
        , gotBrowserNotifRes GotBrowserNotifRes
        ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkCliked
        }
