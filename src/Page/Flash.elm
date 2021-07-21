module Page.Flash exposing (Msg)

import Html.Styled exposing (Html)
import Json.Decode exposing (Value)


type alias FlashMsg msg =
    { time : Int
    , title : String
    , msg : Html msg
    }


type Msg
    = Close
    | GotMsg Value
