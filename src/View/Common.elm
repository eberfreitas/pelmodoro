module View.Common exposing (h1, h2, icon)

import Colors
import Css
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as HtmlAttr
import Types exposing (Theme)


icon : String -> Html msg
icon desc =
    Html.span [ HtmlAttr.class "material-icons-round" ] [ Html.text desc ]


h1 : Theme -> String -> Html msg
h1 theme label =
    Html.h1
        [ HtmlAttr.css
            [ Css.fontSize <| Css.rem 2
            , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
            , Css.marginBottom <| Css.rem 2
            , Css.textAlign Css.center
            ]
        ]
        [ Html.text label ]


h2 : Theme -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
h2 theme label props children =
    Html.h2
        ([ HtmlAttr.css
            [ Css.fontSize <| Css.rem 1.5
            , Css.textAlign <| Css.center
            , Css.color (theme |> Colors.textColor |> Colors.toCssColor)
            ]
         ]
            ++ props
        )
        (Html.text label :: children)
