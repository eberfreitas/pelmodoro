module Elements exposing (h1, h2, h3, icon, styledIcon)

import Color
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Theme.Common
import Theme.Theme as Theme


icon : String -> Html.Html msg
icon desc =
    Html.span [ Attributes.class "material-icons-round" ] [ Html.text desc ]


styledIcon : List Css.Style -> String -> Html.Html msg
styledIcon styles desc =
    Html.span
        [ Attributes.class "material-icons-round"
        , Attributes.css styles
        ]
        [ Html.text desc ]


h1 : Theme.Common.Theme -> String -> Html.Html msg
h1 theme label =
    Html.h1
        [ Attributes.css
            [ Css.fontSize <| Css.rem 2
            , Css.color (theme |> Theme.textColor |> Color.toCssColor)
            , Css.marginBottom <| Css.rem 2
            , Css.textAlign Css.center
            ]
        ]
        [ Html.text label ]


h2 : Theme.Common.Theme -> String -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
h2 theme label props children =
    Html.h2
        (Attributes.css
            [ Css.fontSize <| Css.rem 1.5
            , Css.textAlign <| Css.center
            , Css.color (theme |> Theme.textColor |> Color.toCssColor)
            ]
            :: props
        )
        (Html.text label :: children)


h3 : Theme.Common.Theme -> String -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
h3 theme label props children =
    Html.h3
        (Attributes.css
            [ Css.fontSize <| Css.rem 1
            , Css.textAlign <| Css.center
            , Css.color (theme |> Theme.textColor |> Color.toCssColor)
            ]
            :: props
        )
        (Html.text label :: children)
