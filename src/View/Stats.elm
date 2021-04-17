module View.Stats exposing (render)

import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg)
import View.MiniTimer as MiniTimer


render : Model -> Html Msg
render model =
    Html.div []
        [ MiniTimer.render model ]
