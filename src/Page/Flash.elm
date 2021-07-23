module Page.Flash exposing
    ( FlashMsg
    , Msg
    , empty
    , new
    , setFlash
    , subscriptions
    , update
    , updateFlashTime
    , view
    )

import Color
import Css
import Elements
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
    { a | flash : Maybe (FlashMsg Msg) }


type alias FlashMsg msg =
    { time : Int
    , title : String
    , msg : Html.Html msg
    }



-- VIEW


view : Theme.Common.Theme -> FlashMsg Msg -> Html.Html Msg
view theme { title, msg, time } =
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
        , Html.div [ Attributes.css [ containerStyles, Css.fontSize <| Css.rem 1.25 ] ] [ msg ]
        ]



-- UPDATE


type Msg
    = Close
    | GotMsg Decode.Value


update : Msg -> Model a -> ( Model a, Cmd msg )
update msg model =
    case msg of
        Close ->
            { model | flash = Nothing } |> Misc.withCmd

        GotMsg raw ->
            raw
                |> Decode.decodeValue decodeFlashMsg
                |> Result.map (\f -> { model | flash = Just f })
                |> Result.withDefault model
                |> Misc.withCmd



-- HELPERS


new : String -> Html.Html msg -> FlashMsg msg
new title content =
    FlashMsg 15 title content


empty : FlashMsg msg
empty =
    FlashMsg 0 "" (Html.text "")


setFlash : Maybe (FlashMsg Msg) -> Model a -> Model a
setFlash flashMsg model =
    flashMsg
        |> Maybe.map (\f -> { model | flash = Just f })
        |> Maybe.withDefault model


updateFlashTime : FlashMsg msg -> Maybe (FlashMsg msg)
updateFlashTime ({ time } as msg) =
    if (time - 1) < 1 then
        Nothing

    else
        Just { msg | time = time - 1 }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.gotFlashMsg GotMsg



-- CODECS


decodeFlashMsg : Decode.Decoder (FlashMsg msg)
decodeFlashMsg =
    Decode.map2
        (\title msg -> new title (Html.div [] [ Html.text msg ]))
        (Decode.field "title" Decode.string)
        (Decode.field "msg" Decode.string)
