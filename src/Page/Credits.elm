module Page.Credits exposing (Model, view)

import Color
import Css
import Elements
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Page.MiniTimer as MiniTimer
import Session
import Settings
import Theme



-- MODEL


type alias Model a =
    { a
        | sessions : List Session.RoundType
        , settings : Settings.Settings
        , active : Session.ActiveRound
    }



-- VIEW


view : Model a -> Html.Html msg
view ({ settings } as model) =
    let
        anchorStyle : Css.Style
        anchorStyle =
            Css.batch
                [ Css.color (settings.theme |> Theme.textColor |> Color.toCssColor)
                , Css.fontWeight Css.bold
                ]

        sectionStyle : Css.Style
        sectionStyle =
            Css.batch
                [ Css.lineHeight <| Css.num 1.5
                , Css.marginBottom <| Css.rem 2
                , Css.textAlign Css.center
                ]

        anchor : String -> String -> Html.Html msg
        anchor url text =
            Html.a
                [ Attributes.href url
                , Attributes.target "_blank"
                , Attributes.css [ anchorStyle ]
                ]
                [ Html.text text ]

        strong : String -> Html.Html msg
        strong text =
            Html.strong [] [ Html.text text ]

        h2 : String -> Html.Html msg
        h2 text =
            Elements.h2 settings.theme text [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ] []
    in
    Html.div []
        [ MiniTimer.view model
        , Html.div
            [ Attributes.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.maxWidth <| Css.px 520
                ]
            ]
            [ Elements.h1 settings.theme "Credits"
            , Html.div
                [ Attributes.css [ sectionStyle ] ]
                [ Html.text "Created by "
                , anchor "https://www.eberfdias.com" "Éber F. Dias"
                , Html.text " with "
                , anchor "https://elm-lang.org/" "Elm"
                , Html.text " (and other things). You can check the source code @ "
                , anchor "https://github.com/eberfreitas/pelmodoro" "GitHub"
                , Html.text "."
                ]
            , h2 "Themes"
            , Html.div
                [ Attributes.css [ sectionStyle ] ]
                [ Html.p []
                    [ strong "Gruvbox"
                    , Html.text " originally by "
                    , anchor "https://github.com/morhetz/gruvbox" "Pavel Pertsev"
                    ]
                , Html.p []
                    [ strong "Dracula"
                    , Html.text " originally by "
                    , anchor "https://github.com/dracula/dracula-theme" "Zeno Rocha"
                    ]
                , Html.p []
                    [ strong "Nord"
                    , Html.text " originally by "
                    , anchor "https://github.com/arcticicestudio/nord" "Arctic Ice Studio"
                    ]
                ]
            , h2 "Sounds"
            , Html.div
                [ Attributes.css [ sectionStyle ] ]
                [ Html.p []
                    [ strong "Wind Chimes, A.wav"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/InspectorJ/sounds/353194/" "InspectorJ"
                    ]
                , Html.p []
                    [ strong "Alarm Bell"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/DDmyzik/sounds/460262/" "DDmyzik"
                    ]
                , Html.p []
                    [ strong "Alarm1.mp3"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/kwahmah_02/sounds/250629/" "kwahmah_02"
                    ]
                , Html.p []
                    [ strong "bong.wav"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/OtisJames/sounds/215774/" "OtisJames"
                    ]
                , Html.p []
                    [ strong "Relaxing Percussion.wav"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/AndreAngelo/sounds/246201/" "AndreAngelo"
                    ]
                , Html.p []
                    [ strong "Birdsong Singleshot Denoised Wakeup Alarm"
                    , Html.text " by "
                    , anchor "https://freesound.org/people/unfa/sounds/186024/" "unfa"
                    ]
                ]
            , Html.div
                [ Attributes.css [ sectionStyle ] ]
                [ anchor "https://www.buymeacoffee.com/eberfre" "🍕 buy me a pizza"
                ]
            ]
        ]
