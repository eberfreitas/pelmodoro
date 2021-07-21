module Page.Flash exposing (FlashMsg, Msg, empty, new, setFlash, updateFlashTime)

import Html.Styled as Html
import Json.Decode exposing (Value)


type alias FlashMsg msg =
    { time : Int
    , title : String
    , msg : Html.Html msg
    }


type Msg
    = Close
    | GotMsg Value


type alias Model a msg =
    { a | flash : Maybe (FlashMsg msg) }


new : String -> Html.Html msg -> FlashMsg msg
new title content =
    FlashMsg 15 title content


empty : FlashMsg msg
empty =
    FlashMsg 0 "" (Html.text "")


setFlash : Maybe (FlashMsg msg) -> Model a msg -> Model a msg
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
