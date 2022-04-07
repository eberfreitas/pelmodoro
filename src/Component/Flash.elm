module Component.Flash exposing
    ( Msg
    , subscriptions
    , update
    , view
    )

import Color
import Css
import Elements
import Flash
import Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Misc
import Ports
import Theme.Common
import Theme.Theme as Theme



-- MODEL


type alias Model a =
    { a | global : Global.Global }



-- VIEW


view : Theme.Common.Theme -> Flash.Flash -> Html.Html Msg
view theme flash =
    case flash of
        Just { title, time, msg } ->
            let
                containerStyles =
                    Css.batch
                        [ Css.padding <| Css.rem 1
                        , Css.backgroundColor (theme |> Theme.foregroundColor |> Color.toCssColor)
                        , Css.color (theme |> Theme.contrastColor |> Color.toCssColor)
                        , Css.margin2 Css.zero Css.auto
                        , Css.width <| Css.pct 100
                        , Css.maxWidth <| Css.rem 30
                        , Css.position Css.relative
                        ]
            in
            Html.div
                [ Attributes.css
                    [ Css.position Css.absolute
                    , Css.top Css.zero
                    , Css.width <| Css.pct 100
                    ]
                ]
                [ Html.div
                    [ Attributes.css [ containerStyles, Css.marginBottom <| Css.px 1 ] ]
                    [ Html.div [ Attributes.css [ Css.fontWeight Css.bold ] ] [ Html.text title ]
                    , Html.div
                        [ Attributes.css
                            [ Css.position Css.absolute
                            , Css.right <| Css.rem 0.8
                            , Css.top <| Css.rem 0.8
                            ]
                        ]
                        [ Html.span [ Attributes.css [ Css.fontSize <| Css.rem 0.75 ] ] [ Html.text (String.fromInt time) ]
                        , Html.button
                            [ Events.onClick Close
                            , Attributes.css
                                [ Css.margin Css.zero
                                , Css.border <| Css.rem 0
                                , Css.backgroundColor Css.transparent
                                , Css.color (theme |> Theme.contrastColor |> Color.toCssColor)
                                , Css.display Css.inlineBlock
                                , Css.marginLeft <| Css.rem 0.5
                                , Css.verticalAlign Css.middle
                                , Css.cursor Css.pointer
                                ]
                            ]
                            [ Elements.icon "highlight_off" ]
                        ]
                    ]
                , Html.div [ Attributes.css [ containerStyles, Css.fontSize <| Css.rem 1.25 ] ] [ Html.text msg ]
                ]

        Nothing ->
            Html.text ""



-- UPDATE


type Msg
    = Close
    | GotMsg Decode.Value


update : Msg -> Model a -> ( Model a, Cmd msg )
update msg ({ global } as model) =
    case msg of
        Close ->
            let
                newGlobal =
                    { global | flash = Nothing }
            in
            { model | global = newGlobal } |> Misc.withCmd

        GotMsg raw ->
            raw
                |> Decode.decodeValue decodeFlashMsg
                |> Result.map (\f -> Global.setFlash f model)
                |> Result.withDefault model
                |> Misc.withCmd



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.gotFlashMsg GotMsg



-- CODECS


decodeFlashMsg : Decode.Decoder Flash.Flash
decodeFlashMsg =
    Decode.map2
        (\title msg -> Just <| Flash.new title msg)
        (Decode.field "title" Decode.string)
        (Decode.field "msg" Decode.string)
