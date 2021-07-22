module Page.Timer exposing (Msg, secondsToDisplay, subscriptions, update, view)

import Html.Styled as Html
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Misc
import Page.Flash as Flash
import Page.Settings as Settings
import Page.Spotify as Spotify
import Ports
import Session
import Time



-- MODEL


type alias Model a =
    { a
        | time : Time.Posix
        , playing : Bool
        , active : Session.Active
        , settings : Settings.Settings
        , sessions : List Session.SessionDef
        , uptime : Seconds
        , flash : Maybe (Flash.FlashMsg Flash.Msg)
        , sentimentSession : Maybe Session.Session
    }


type alias Seconds =
    Int


type alias EvalResult msg =
    { active : Session.Active
    , playing : Bool
    , flash : Maybe (Flash.FlashMsg msg)
    , cmd : Cmd msg
    , sentimentSession : Maybe Session.Session
    }



-- VIEW


view : a -> Html.Html msg
view _ =
    Html.text ""



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
                        , Spotify.play settings.spotify
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

        SetSentiment _ _ ->
            Debug.todo ""



-- HELPERS


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


evalElapsedTime : Model a -> EvalResult msg
evalElapsedTime { active, sessions, settings, time } =
    if Session.secondsLeft active == 0 then
        let
            nextIndex =
                active.index + 1

            ( newActive, playing ) =
                Session.rollActiveSession time nextIndex settings.flow sessions

            ( flashMsg, notificationMsg ) =
                Session.sessionChangeToFlash time active.session.def newActive.session.def

            sentimentSession =
                if active.session.def |> Session.isWork then
                    Just active.session

                else
                    Nothing

            notificationCmd =
                { sound = Settings.alarmSoundToString settings.alarmSound
                , msg = notificationMsg
                , config = settings.notifications
                }
                    |> encodeNotificationConfig
                    |> Ports.notify

            spotifyCmd =
                if Session.isWork active.session.def then
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
tick posix ({ playing, flash, active } as model) =
    if playing then
        let
            newState =
                evalElapsedTime model
        in
        { model
            | active = newState.active
            , playing = newState.playing
            , flash = flash |> Maybe.andThen Flash.updateFlashTime
        }
            |> setupSentimentSession newState.sentimentSession newState.active.session.def
            |> updateTime posix
            |> Flash.setFlash newState.flash
            |> Misc.withCmd
            |> Misc.addCmd newState.cmd
            |> Misc.addCmd (Session.saveActive active)

    else
        { model | flash = flash |> Maybe.andThen Flash.updateFlashTime }
            |> updateTime posix
            |> Misc.withCmd



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
