module View.Settings exposing (render)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Model exposing (Model)
import Msg exposing (Msg(..))
import Themes.Theme as Theme
import Tuple.Trio as Trio
import Types exposing (Continuity(..), Spotify(..), Theme(..))
import View.Common as Common
import View.MiniTimer as MiniTimer


render : Model -> Html Msg
render ({ settings } as model) =
    let
        labelStyle =
            Css.batch
                [ Css.fontSize <| Css.rem 1.2
                , Css.marginBottom <| Css.rem 1
                , Css.fontWeight <| Css.bold
                ]

        buttonStyle =
            Css.batch
                [ Css.borderStyle Css.none
                , Css.backgroundColor <| (settings.theme |> Theme.foregroundColor |> Colors.toCssColor)
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.color <| (settings.theme |> Theme.backgroundColor |> Colors.toCssColor)
                , Css.outline Css.zero
                , Css.cursor Css.pointer
                ]

        largeButtonStyle =
            Css.batch
                [ buttonStyle
                , Css.display Css.block
                , Css.width <| Css.pct 100
                , Css.textAlign Css.center
                , Css.textDecoration Css.none
                , Css.paddingTop <| Css.rem 1
                , Css.fontSize <| Css.rem 1
                ]

        settingDisplayStyle =
            Css.batch
                [ Css.height <| Css.rem 3
                , Css.backgroundColor (settings.theme |> Theme.contrastColor |> Colors.toCssColor)
                , Css.color (settings.theme |> Theme.textColor |> Colors.toCssColor)
                , Css.padding2 (Css.rem 1) (Css.rem 0)
                , Css.width (Css.calc (Css.pct 100) Css.minus (Css.rem 6))
                , Css.textAlign Css.center
                ]

        selectStyle =
            Css.batch
                [ Css.property "appearance" "none"
                , Css.borderStyle Css.none
                , Css.fontFamilies [ "Montserrat" ]
                , Css.fontSize <| Css.rem 1
                , Css.padding <| Css.rem 1
                , Css.width <| Css.pct 100
                , Css.cursor Css.pointer
                , Css.color (settings.theme |> Theme.textColor |> Colors.toCssColor)
                , Css.backgroundColor (settings.theme |> Theme.contrastColor |> Colors.toCssColor)
                , Css.backgroundRepeat Css.noRepeat
                , Css.backgroundPosition2 (Css.pct 95) (Css.pct 50)
                , Css.property "background-image"
                    "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' height='24px' viewBox='0 0 24 24' width='24px' fill='%23000000'><path d='M0 0h24v24H0V0z' fill='none'/><path d='M8.71 11.71l2.59 2.59c.39.39 1.02.39 1.41 0l2.59-2.59c.63-.63.18-1.71-.71-1.71H9.41c-.89 0-1.33 1.08-.7 1.71z'/></svg>\")"
                ]

        atLeast target num =
            if num < target then
                target

            else
                num

        atMost target num =
            if num > target then
                target

            else
                num

        inMinutes seconds =
            seconds // 60

        inputContainer label input =
            Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text label ]
                , input
                ]

        numberInput min max msg num =
            Html.div
                [ HtmlAttr.css [ Css.displayFlex ] ]
                [ Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (num - 1 |> atLeast min |> msg) ] [ Common.icon "remove" ]
                , Html.div
                    [ HtmlAttr.css [ settingDisplayStyle ] ]
                    [ Html.text <| String.fromInt num ]
                , Html.button [ HtmlAttr.css [ buttonStyle ], Event.onClick (num + 1 |> atMost max |> msg) ] [ Common.icon "add" ]
                ]

        selectInput fn msg pairs =
            Html.div []
                [ Html.select [ HtmlAttr.css [ selectStyle ], Event.onInput msg ]
                    (pairs
                        |> List.map
                            (\(( _, v, l ) as def) ->
                                Html.option
                                    [ HtmlAttr.value v, HtmlAttr.selected (fn def) ]
                                    [ Html.text l ]
                            )
                    )
                ]

        toTrio ( a, b ) =
            ( a, b, b )
    in
    Html.div []
        [ MiniTimer.render model
        , Html.div
            [ HtmlAttr.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.width <| Css.px 280
                ]
            ]
            [ Common.h1 settings.theme "Settings"
            , inputContainer "Rounds" <| numberInput 1 8 ChangeRounds settings.rounds
            , inputContainer "Session duration" <| numberInput 1 60 ChangeActivity <| inMinutes settings.activity
            , inputContainer "Break duration" <| numberInput 1 60 ChangeBreak <| inMinutes settings.break
            , inputContainer "Long break duration" <| numberInput 1 60 ChangeLongBreak <| inMinutes settings.longBreak
            , inputContainer "Rounds continuity" <|
                selectInput
                    (Trio.first >> (==) settings.continuity)
                    ChangeContinuity
                    (Model.continuityPairs |> List.map toTrio)
            , inputContainer "Color theme" <|
                selectInput
                    (Trio.first >> (==) settings.theme)
                    ChangeTheme
                    (Model.themePairs |> List.map toTrio)
            , Html.div [ HtmlAttr.css [ Css.marginBottom <| Css.rem 2 ] ]
                [ Html.div [ HtmlAttr.css [ labelStyle ] ] [ Html.text "Spotify" ]
                , Html.div []
                    (case settings.spotify of
                        NotConnected url ->
                            [ Html.a
                                [ HtmlAttr.href url
                                , HtmlAttr.css [ largeButtonStyle ]
                                ]
                                [ Html.text "Connect to Spotify" ]
                            ]

                        ConnectionError url ->
                            [ Html.p [ HtmlAttr.css [ Css.marginBottom <| Css.rem 1 ] ]
                                [ Html.text "There was an error trying to connect. Please, try again!" ]
                            , Html.a
                                [ HtmlAttr.href url
                                , HtmlAttr.css [ largeButtonStyle ]
                                ]
                                [ Html.text "Connect to Spotify" ]
                            ]

                        Connected playlists current ->
                            [ Html.select [ HtmlAttr.css [ selectStyle ], Event.onInput ChangePlaylist ]
                                (playlists
                                    |> List.sortBy Tuple.second
                                    |> List.map
                                        (\( uri, title ) ->
                                            Html.option
                                                [ HtmlAttr.value uri, HtmlAttr.selected (current == Just uri) ]
                                                [ Html.text title ]
                                        )
                                    |> (::) (Html.option [ HtmlAttr.value "" ] [ Html.text "--" ])
                                    |> (::) (Html.option [ HtmlAttr.value "", HtmlAttr.selected (current == Nothing) ] [ Html.text "Don't play anything" ])
                                )
                            , Html.button
                                [ Event.onClick SpotifyRefresh
                                , HtmlAttr.css [ largeButtonStyle, Css.marginTop <| Css.rem 1, Css.paddingTop <| Css.zero ]
                                ]
                                [ Html.text "Refresh playlists" ]
                            , Html.button
                                [ Event.onClick SpotifyDisconnect
                                , HtmlAttr.css [ largeButtonStyle, Css.marginTop <| Css.rem 1, Css.paddingTop <| Css.zero ]
                                ]
                                [ Html.text "Disconnect" ]
                            ]

                        Uninitialized ->
                            [ Html.text "Can't connect to Spotify" ]
                    )
                ]
            ]
        ]
