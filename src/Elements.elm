module Elements exposing
    ( checkbox
    , h1
    , h2
    , h3
    , icon
    , inputContainer
    , labelStyle
    , largeButton
    , largeLinkButton
    , numberInput
    , selectInput
    , selectStyle
    , simpleSeparator
    , styledIcon
    )

import Color
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Theme
import Theme.Common


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


buttonStyle : Theme.Common.Theme -> Css.Style
buttonStyle theme =
    Css.batch
        [ Css.borderStyle Css.none
        , Css.backgroundColor <| (theme |> Theme.foregroundColor |> Color.toCssColor)
        , Css.width <| Css.rem 3
        , Css.height <| Css.rem 3
        , Css.color <| (theme |> Theme.backgroundColor |> Color.toCssColor)
        , Css.outline Css.zero
        , Css.cursor Css.pointer
        ]


largeButtonStyle : Theme.Common.Theme -> Css.Style
largeButtonStyle theme =
    Css.batch
        [ buttonStyle theme
        , Css.display Css.block
        , Css.width <| Css.pct 100
        , Css.textAlign Css.center
        , Css.textDecoration Css.none
        , Css.fontSize <| Css.rem 1
        , Css.fontFamilies [ "Montserrat" ]
        ]


largeButton : Theme.Common.Theme -> msg -> List (Html.Html msg) -> Html.Html msg
largeButton theme msg body =
    Html.button
        [ Events.onClick msg
        , Attributes.css [ largeButtonStyle theme ]
        ]
        body


largeLinkButton : Theme.Common.Theme -> String -> String -> Html.Html msg
largeLinkButton theme url label =
    Html.a
        [ Attributes.href url
        , Attributes.css
            [ largeButtonStyle theme
            , Css.paddingTop <| Css.rem 1
            ]
        ]
        [ Html.text label ]


selectStyle : Theme.Common.Theme -> Css.Style
selectStyle theme =
    Css.batch
        [ Css.property "appearance" "none"
        , Css.borderStyle Css.none
        , Css.fontFamilies [ "Montserrat" ]
        , Css.fontSize <| Css.rem 1
        , Css.padding <| Css.rem 1
        , Css.width <| Css.pct 100
        , Css.cursor Css.pointer
        , Css.color (theme |> Theme.textColor |> Color.toCssColor)
        , Css.backgroundColor (theme |> Theme.contrastColor |> Color.toCssColor)
        , Css.backgroundRepeat Css.noRepeat
        , Css.backgroundPosition2 (Css.pct 95) (Css.pct 50)
        , Css.property "background-image"
            "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' height='24px' viewBox='0 0 24 24' width='24px' fill='%23000000'><path d='M0 0h24v24H0V0z' fill='none'/><path d='M8.71 11.71l2.59 2.59c.39.39 1.02.39 1.41 0l2.59-2.59c.63-.63.18-1.71-.71-1.71H9.41c-.89 0-1.33 1.08-.7 1.71z'/></svg>\")"
        ]


selectInput :
    Theme.Common.Theme
    -> (( a, String, String ) -> Bool)
    -> (String -> msg)
    -> List ( a, String, String )
    -> Html.Html msg
selectInput theme selectedFn toMsg trios =
    Html.div []
        [ Html.select [ Attributes.css [ selectStyle theme ], Events.onInput toMsg ]
            (trios
                |> List.map
                    (\(( _, v, l ) as def) ->
                        Html.option
                            [ Attributes.value v, Attributes.selected (selectedFn def) ]
                            [ Html.text l ]
                    )
            )
        ]


labelStyle : Css.Style
labelStyle =
    Css.batch
        [ Css.fontSize <| Css.rem 1.2
        , Css.marginBottom <| Css.rem 1
        , Css.fontWeight <| Css.bold
        ]


checkbox : Theme.Common.Theme -> Bool -> msg -> String -> Html.Html msg
checkbox theme val msg label =
    let
        icon_ : String
        icon_ =
            if val then
                "check_box"

            else
                "check_box_outline_blank"
    in
    Html.div []
        [ Html.button
            [ Attributes.css
                [ Css.backgroundColor Css.transparent
                , Css.border Css.zero
                , Css.padding Css.zero
                , Css.margin Css.zero
                , Css.verticalAlign Css.middle
                , Css.cursor Css.pointer
                , Css.color (theme |> Theme.foregroundColor |> Color.toCssColor)
                ]
            , Events.onClick msg
            ]
            [ icon icon_ ]
        , Html.span [ Events.onClick msg ] [ Html.text label ]
        ]


atLeast : Int -> Int -> Int
atLeast target num =
    if num < target then
        target

    else
        num


atMost : Int -> Int -> Int
atMost target num =
    if num > target then
        target

    else
        num


numberInputStyle : Theme.Common.Theme -> Css.Style
numberInputStyle theme =
    Css.batch
        [ Css.height <| Css.rem 3
        , Css.backgroundColor (theme |> Theme.contrastColor |> Color.toCssColor)
        , Css.color (theme |> Theme.textColor |> Color.toCssColor)
        , Css.padding2 (Css.rem 1) (Css.rem 0)
        , Css.width (Css.calc (Css.pct 100) Css.minus (Css.rem 6))
        , Css.textAlign Css.center
        ]


numberInput : Theme.Common.Theme -> Int -> Int -> (Int -> msg) -> Int -> Html.Html msg
numberInput theme min max msg val =
    Html.div
        [ Attributes.css [ Css.displayFlex ] ]
        [ Html.button
            [ Attributes.css [ buttonStyle theme ], Events.onClick (val - 1 |> atLeast min |> msg) ]
            [ icon "remove" ]
        , Html.div
            [ Attributes.css [ numberInputStyle theme ] ]
            [ Html.text <| String.fromInt val ]
        , Html.button
            [ Attributes.css [ buttonStyle theme ], Events.onClick (val + 1 |> atMost max |> msg) ]
            [ icon "add" ]
        ]


inputContainer : String -> Html.Html msg -> Html.Html msg
inputContainer label input =
    Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 2 ] ]
        [ Html.div [ Attributes.css [ labelStyle ] ] [ Html.text label ]
        , input
        ]


simpleSeparator : Html.Html msg -> Html.Html msg
simpleSeparator body =
    Html.div [ Attributes.css [ Css.marginBottom <| Css.rem 1 ] ] [ body ]
