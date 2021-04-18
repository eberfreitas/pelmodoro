module View.Common exposing (header, icon)

import Colors
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Types exposing (Theme)


icon : String -> Html msg
icon desc =
    Html.span [ HtmlAttr.class "material-icons-round" ] [ Html.text desc ]


header : Theme -> String -> Html msg
header theme label =
    Html.h1
        [ HtmlAttr.css
            [ Css.fontSize <| Css.rem 2
            , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
            , Css.marginBottom <| Css.rem 2
            , Css.textAlign Css.center
            ]
        ]
        [ Html.text label ]
