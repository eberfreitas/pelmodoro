module View.Flash exposing (render)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Event
import Msg exposing (Msg(..))
import Themes.Theme as Theme
import Themes.Types exposing (Theme)
import Types exposing (FlashMsg)
import View.Common as Common


render : Theme -> FlashMsg Msg -> Html Msg
render theme { title, msg, time } =
    let
        containerStyles =
            Css.batch
                [ Css.padding <| Css.rem 1
                , Css.backgroundColor (theme |> Theme.foregroundColor |> Colors.toCssColor)
                , Css.color (theme |> Theme.contrastColor |> Colors.toCssColor)
                , Css.margin2 Css.zero Css.auto
                , Css.width <| Css.pct 100
                , Css.maxWidth <| Css.rem 30
                , Css.position Css.relative
                ]
    in
    Html.div
        [ HtmlAttr.css
            [ Css.position Css.absolute
            , Css.top Css.zero
            , Css.width <| Css.pct 100
            ]
        ]
        [ Html.div
            [ HtmlAttr.css [ containerStyles, Css.marginBottom <| Css.px 1 ] ]
            [ Html.div [ HtmlAttr.css [ Css.fontWeight Css.bold ] ] [ Html.text title ]
            , Html.div
                [ HtmlAttr.css
                    [ Css.position Css.absolute
                    , Css.right <| Css.rem 0.8
                    , Css.top <| Css.rem 0.8
                    ]
                ]
                [ Html.span [ HtmlAttr.css [ Css.fontSize <| Css.rem 0.75 ] ] [ Html.text (String.fromInt time) ]
                , Html.button
                    [ Event.onClick CloseFlashMsg
                    , HtmlAttr.css
                        [ Css.margin Css.zero
                        , Css.border <| Css.rem 0
                        , Css.backgroundColor Css.transparent
                        , Css.color (theme |> Theme.contrastColor |> Colors.toCssColor)
                        , Css.display Css.inlineBlock
                        , Css.marginLeft <| Css.rem 0.5
                        , Css.verticalAlign Css.middle
                        , Css.cursor Css.pointer
                        ]
                    ]
                    [ Common.icon "highlight_off" ]
                ]
            ]
        , Html.div [ HtmlAttr.css [ containerStyles, Css.fontSize <| Css.rem 1.25 ] ] [ msg ]
        ]
