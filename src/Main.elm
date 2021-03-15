port module Main exposing (main)

import Browser
import Colors
import Css
import Helpers
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListEx
import Model exposing (Continuity(..), Current, Interval, Model, Page(..))
import Msg exposing (Msg(..))
import Platform exposing (Program)
import Task
import Time exposing (Posix)
import View.Timer as Timer


port notify : () -> Cmd msg


port persistCurrent : E.Value -> Cmd msg


port persistSettings : E.Value -> Cmd msg


type alias Flags =
    { current : D.Value
    , settings : D.Value
    }


init : Flags -> ( Model, Cmd Msg )
init { current, settings } =
    let
        baseModel =
            Model.default

        newCurrent =
            case D.decodeValue Model.decodeCurrent current of
                Ok curr ->
                    curr

                Err _ ->
                    baseModel.current

        newSettings =
            case D.decodeValue Model.decodeSettings settings of
                Ok settings_ ->
                    settings_

                Err _ ->
                    baseModel.settings
    in
    ( { baseModel | current = newCurrent, settings = newSettings }, Task.perform AdjustTimeZone Time.here )


view : Model -> Html Msg
view model =
    Html.div
        [ HtmlAttr.css
            [ Css.width <| Css.vw 100.0
            , Css.height <| Css.vh 100.0
            , Css.position Css.relative
            , Css.backgroundColor <| (model.settings.theme |> Colors.backgroundColor |> Colors.toCssColor)
            ]
        ]
        [ renderPage model
        ]


renderPage : Model -> Html Msg
renderPage model =
    case model.page of
        TimerPage ->
            Timer.render model

        _ ->
            Html.text "other pages"


evalElapsedTime : Posix -> Current -> Continuity -> List Interval -> ( Current, Bool, Cmd msg )
evalElapsedTime now current repeat intervals =
    if Model.currentSecondsLeft current == 0 then
        let
            firstInterval =
                intervals |> Model.firstInverval

            nextIdx =
                current.index + 1

            ( current_, playing ) =
                case ( intervals |> ListEx.getAt nextIdx, repeat ) of
                    ( Nothing, FullCont ) ->
                        ( Current 0 (Model.cycleBuild firstInterval (Just now)) 0, True )

                    ( Nothing, _ ) ->
                        ( Current 0 (Model.cycleBuild firstInterval Nothing) 0, False )

                    ( Just nextInterval, NoCont ) ->
                        ( Current nextIdx (Model.cycleBuild nextInterval Nothing) 0, False )

                    ( Just nextInterval, _ ) ->
                        ( Current nextIdx (Model.cycleBuild nextInterval (Just now)) 0, True )
        in
        ( current_, playing, notify () )

    else
        ( Model.currentAddElapsed 1 current, True, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        done m =
            ( m, Cmd.none )

        persistCurrent_ cmds model_ =
            model_.current
                |> Model.encodeCurrent
                |> persistCurrent
                |> Helpers.flip (::) cmds
                |> Cmd.batch
                |> Tuple.pair model_

        persistSettings_ cmds model_ =
            model_.settings
                |> Model.encodeSettings
                |> persistSettings
                |> Helpers.flip (::) cmds
                |> Cmd.batch
                |> Tuple.pair model_
    in
    case msg of
        NoOp ->
            done model

        Tick posix ->
            let
                updateTime model_ =
                    { model_ | time = posix, uptime = model_.uptime + 1 }
            in
            if model.playing == True then
                let
                    ( newCurrent, newPlaying, cmd ) =
                        evalElapsedTime model.time model.current model.settings.continuity model.intervals

                    newLog =
                        if cmd /= Cmd.none then
                            model.log |> Model.cycleLog model.time model.current

                        else
                            model.log
                in
                { model | current = newCurrent, playing = newPlaying, log = newLog }
                    |> updateTime
                    |> persistCurrent_ [ cmd ]

            else
                model |> updateTime |> done

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        Pause ->
            done { model | playing = False }

        Play ->
            let
                { index, cycle, elapsed } =
                    model.current

                newCurrent =
                    if elapsed == 0 then
                        Current index (Model.cycleStart model.time cycle) 0

                    else
                        model.current
            in
            { model | playing = True, current = newCurrent } |> persistCurrent_ [ Cmd.none ]

        Skip ->
            let
                { index } =
                    model.current

                ( newIndex, newInterval ) =
                    case ListEx.getAt (index + 1) model.intervals of
                        Just next ->
                            ( index + 1, next )

                        Nothing ->
                            ( 0, model.intervals |> Model.firstInverval )

                newCurrent =
                    Current newIndex (Model.cycleBuild newInterval Nothing) 0

                newLog =
                    model.log |> Model.cycleLog model.time model.current
            in
            { model | current = newCurrent, playing = False, log = newLog } |> persistCurrent_ [ Cmd.none ]

        Restart ->
            let
                newLog =
                    model.log |> Model.cycleLog model.time model.current

                newCurrent =
                    Current 0 (Model.cycleBuild (Model.firstInverval model.intervals) Nothing) 0
            in
            { model | current = newCurrent, log = newLog, playing = False } |> persistCurrent_ [ Cmd.none ]

        SetCont cont ->
            model |> Model.mapSettings (\s -> { s | continuity = cont }) |> done

        ChangeSettings settings ->
            { model | settings = settings } |> persistSettings_ [ Cmd.none ]


subs : Model -> Sub Msg
subs _ =
    Time.every 1000 Tick


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subs
        }
