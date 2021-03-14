port module Main exposing (main)

import Browser
import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import List.Extra as ListEx
import Model exposing (Continuity(..), Current, Interval, Model, Page(..))
import Msg exposing (Msg(..))
import Platform exposing (Program)
import Task
import Time exposing (Posix)
import View.Timer as Timer


port notify : () -> Cmd msg


init : () -> ( Model, Cmd Msg )
init () =
    ( Model.default, Task.perform AdjustTimeZone Time.here )


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

            idx =
                current.index

            nextIdx =
                idx + 1

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
    in
    case msg of
        NoOp ->
            done model

        Tick posix ->
            if model.playing == True then
                let
                    ( newCurrent, newPlaying, cmd ) =
                        evalElapsedTime model.time model.current model.continuity model.intervals

                    newLog =
                        if cmd /= Cmd.none then
                            model.log |> Model.cycleLog model.time model.current

                        else
                            model.log
                in
                ( { model | current = newCurrent, playing = newPlaying, time = posix, uptime = model.uptime + 1, log = newLog }, cmd )

            else
                done { model | time = posix, uptime = model.uptime + 1 }

        AdjustTimeZone newZone ->
            done { model | zone = newZone }

        Pause ->
            done { model | playing = False }

        Play ->
            let
                { index, cycle, elapsed } =
                    model.current

                current =
                    if elapsed == 0 then
                        Current index (Model.cycleStart model.time cycle) 0

                    else
                        model.current
            in
            done { model | playing = True, current = current }

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
                    if model.current.elapsed /= 0 then
                        model.log |> Model.cycleLog model.time model.current

                    else
                        model.log
            in
            done { model | current = newCurrent, playing = False, log = newLog }

        Restart ->
            let
                newLog =
                    if model.current.elapsed /= 0 then
                        model.log |> Model.cycleLog model.time model.current

                    else
                        model.log

                newCurrent =
                    Current 0 (Model.cycleBuild (Model.firstInverval model.intervals) Nothing) 0
            in
            done { model | current = newCurrent, log = newLog, playing = False }

        SetCont cont ->
            done { model | continuity = cont }


subs : Model -> Sub Msg
subs _ =
    Time.every 1000 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subs
        }
