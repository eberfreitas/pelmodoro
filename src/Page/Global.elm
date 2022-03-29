module Page.Global exposing (Msg, subscriptions, update)

import Env
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Misc
import Page.Flash as Flash
import Page.Spotify as Spotify
import Ports
import Quotes
import Sessions
import Settings
import Time



-- MODEL


type alias Model a =
    { a
        | env : Env.Env
        , settings : Settings.Settings
        , sessions : Sessions.Sessions
        , flash : Flash.Flash
    }


type alias EvalResult msg =
    { active : Sessions.Active
    , playing : Bool
    , flash : Maybe (Flash.FlashMsg msg)
    , cmd : Cmd msg
    , previousRound : Maybe Sessions.Session
    }



-- UPDATE


type Msg
    = Tick Decode.Value


update : Msg -> Model a -> ( Model a, Cmd msg )
update msg model =
    case msg of
        Tick raw ->
            case Decode.decodeValue Decode.int raw of
                Ok millis ->
                    model |> tick (Time.millisToPosix millis)

                Err _ ->
                    Misc.withCmd model



-- HELPERS


tick : Time.Posix -> Model a -> ( Model a, Cmd msg )
tick posix ({ flash, settings, sessions } as model) =
    if sessions.playing then
        let
            newState =
                evalElapsedTime model

            setFlashFn =
                if settings.notifications.inApp then
                    Flash.setFlash newState.flash

                else
                    identity

            newSessions =
                { sessions | active = newState.active, playing = newState.playing }
        in
        { model
            | sessions = newSessions
            , flash = flash |> Maybe.andThen Flash.updateFlashTime
        }
            |> setupSentimentSession newState.previousRound newState.active.session.def
            |> updateTime posix
            |> setFlashFn
            |> Misc.withCmd
            |> Misc.addCmd newState.cmd
            |> Misc.addCmd (Sessions.saveActive sessions.active)

    else
        { model | flash = flash |> Maybe.andThen Flash.updateFlashTime }
            |> updateTime posix
            |> Misc.withCmd


evalElapsedTime : Model a -> EvalResult msg
evalElapsedTime { sessions, settings, env } =
    if Sessions.secondsLeft sessions.active == 0 then
        let
            nextIndex =
                sessions.active.index + 1

            ( newActive, playing ) =
                rollActiveSession env.time nextIndex settings.flow sessions.sessions

            ( flashMsg, notificationMsg ) =
                sessionChangeToFlash env.time sessions.active.session.def newActive.session.def

            previousRound =
                if sessions.active.session.def |> Sessions.isWork then
                    Just sessions.active.session

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
                if Sessions.isWork newActive.session.def then
                    Spotify.playCmd settings.spotify

                else
                    Spotify.pauseCmd settings.spotify

            logCmd =
                Sessions.logSession env.time sessions.active
        in
        EvalResult
            newActive
            playing
            (Just flashMsg)
            (Cmd.batch [ notificationCmd, spotifyCmd, logCmd ])
            previousRound

    else
        EvalResult (Sessions.addElapsed 1 sessions.active) True Nothing Cmd.none Nothing


updateTime : Time.Posix -> Model a -> Model a
updateTime now ({ env, sessions } as model) =
    { model
        | env = { env | time = now }
        , sessions = { sessions | uptime = sessions.uptime + 1 }
    }


setupSentimentSession :
    Maybe Sessions.Session
    -> Sessions.SessionDef
    -> Model a
    -> Model a
setupSentimentSession session sessionDef model =
    let
        newSession =
            case ( model.previousRound, session, Sessions.isWork sessionDef ) of
                ( _, _, True ) ->
                    Nothing

                ( sentiment, Nothing, _ ) ->
                    sentiment

                ( Nothing, sentiment, _ ) ->
                    sentiment

                _ ->
                    Nothing
    in
    { model | previousRound = newSession }


rollActiveSession :
    Time.Posix
    -> Int
    -> Settings.Flow
    -> List Sessions.SessionDef
    -> ( Sessions.Active, Bool )
rollActiveSession now nextIndex flow sessions =
    let
        firstSession_ =
            sessions |> Sessions.firstSession

        nextActive =
            case sessions |> List.Extra.getAt nextIndex of
                Nothing ->
                    Sessions.Active 0 (Sessions.newSession firstSession_) 0

                Just nextSession ->
                    Sessions.Active nextIndex (Sessions.newSession nextSession) 0
    in
    if Settings.shouldKeepPlaying nextActive.index flow then
        ( { nextActive | session = Sessions.setSessionStart now nextActive.session }
        , True
        )

    else
        ( nextActive, False )


sessionChangeToFlash : Time.Posix -> Sessions.SessionDef -> Sessions.SessionDef -> ( Flash.FlashMsg msg, String )
sessionChangeToFlash now from to =
    case Sessions.sessionChangeToLabel from to of
        "" ->
            ( Flash.empty, "" )

        label ->
            ( Flash.new label (Quotes.randomHtmlQuote now), label )



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
