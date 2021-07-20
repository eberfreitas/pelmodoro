module View.Credits exposing (render)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Model exposing (Model)
import Themes.Theme as Theme
import View.Common as Common
import View.MiniTimer as MiniTimer


render : Model -> Html msg
render ({ settings } as model) =
    let
        anchorStyle =
            Css.batch
                [ Css.color (settings.theme |> Theme.textColor |> Colors.toCssColor)
                , Css.fontWeight Css.bold
                ]

        sectionStyle =
            Css.batch
                [ Css.lineHeight <| Css.num 1.5
                , Css.marginBottom <| Css.rem 2
                , Css.textAlign Css.center
                ]

        anchor url text =
            Html.a
                [ HtmlAttr.href url
                , HtmlAttr.target "_blank"
                , HtmlAttr.css [ anchorStyle ]
                ]
                [ Html.text text ]

        strong text =
            Html.strong [] [ Html.text text ]

        h2 text =
            Common.h2 settings.theme text [ HtmlAttr.css [ Css.marginBottom <| Css.rem 1 ] ] []
    in
    Html.div []
        [ MiniTimer.render model
        , Html.div
            [ HtmlAttr.css
                [ Css.margin2 (Css.rem 2) Css.auto
                , Css.maxWidth <| Css.px 520
                ]
            ]
            [ Common.h1 settings.theme "Credits"
            , Html.div
                [ HtmlAttr.css [ sectionStyle ] ]
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
                [ HtmlAttr.css [ sectionStyle ] ]
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
                [ HtmlAttr.css [ sectionStyle ] ]
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
                [ HtmlAttr.css [ sectionStyle ] ]
                [ anchor "https://www.buymeacoffee.com/eberfre" "🍕 buy me a pizza"
                ]
            ]
        ]
