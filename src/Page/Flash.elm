module Page.Flash exposing
    ( FlashMsg
    , Model
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
import Theme
import Theme.Common



-- MODEL


type alias Model a =
    { a | flash : Maybe FlashMsg }


type alias FlashMsg =
    { time : Int
    , msg : String
    }



-- VIEW


view : Theme.Common.Theme -> FlashMsg -> Html.Html Msg
view theme { msg, time } =
    let
        containerStyles : Css.Style
        containerStyles =
            Css.batch
                [ Css.padding <| Css.rem 1
                , Css.backgroundColor (theme |> Theme.foregroundColor |> Color.toCssColor)
                , Css.color (theme |> Theme.contrastColor |> Color.toCssColor)
                , Css.margin2 Css.zero Css.auto
                , Css.width <| Css.pct 100
                , Css.maxWidth <| Css.rem 40
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
            [ Attributes.css [ containerStyles ] ]
            [ Html.span [] [ Html.text msg ]
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


new : String -> FlashMsg
new content =
    FlashMsg 15 content


empty : FlashMsg
empty =
    FlashMsg 0 ""


setFlash : Maybe FlashMsg -> Model a -> Model a
setFlash flashMsg model =
    flashMsg
        |> Maybe.map (\f -> { model | flash = Just f })
        |> Maybe.withDefault model


updateFlashTime : FlashMsg -> Maybe FlashMsg
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


decodeFlashMsg : Decode.Decoder FlashMsg
decodeFlashMsg =
    Decode.field "msg" Decode.string |> Decode.map new
