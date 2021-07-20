module Tools exposing
    ( continuityFromString
    , continuityToString
    , fromString
    , makePairs
    , notificationsDefault
    , sentimentFromString
    , sentimentToDisplay
    , sentimentToString
    , soundFromString
    , soundToString
    )

import List.Extra as ListEx
import String.Extra as StringEx
import Types exposing (Continuity(..), Notifications, Sentiment(..), Sound(..))


makePairs : (a -> String) -> List a -> List ( a, String )
makePairs fn =
    List.map (\a -> ( a, fn a ))


fromString : List ( a, String ) -> String -> Maybe a
fromString list s =
    list
        |> ListEx.find (Tuple.second >> (==) s)
        |> Maybe.map Tuple.first


continuityToString : Continuity -> String
continuityToString continuity =
    case continuity of
        NoCont ->
            "nocont"

        SimpleCont ->
            "simplecont"

        FullCont ->
            "fullcont"


continuityPairs : List ( Continuity, String )
continuityPairs =
    [ NoCont
    , SimpleCont
    , FullCont
    ]
        |> makePairs continuityToString


continuityFromString : String -> Maybe Continuity
continuityFromString =
    fromString continuityPairs


notificationsDefault : Notifications
notificationsDefault =
    Notifications True True False


sentimentToString : Sentiment -> String
sentimentToString sentiment =
    case sentiment of
        Positive ->
            "positive"

        Neutral ->
            "neutral"

        Negative ->
            "negative"


sentimentPairs : List ( Sentiment, String )
sentimentPairs =
    [ Positive
    , Neutral
    , Negative
    ]
        |> makePairs sentimentToString


sentimentFromString : String -> Maybe Sentiment
sentimentFromString =
    fromString sentimentPairs


sentimentToDisplay : Sentiment -> String
sentimentToDisplay =
    sentimentToString >> StringEx.toSentenceCase


soundToString : Sound -> String
soundToString sound =
    case sound of
        WindChimes ->
            "wind-chimes"


soundPairs : List ( Sound, String )
soundPairs =
    [ WindChimes ] |> makePairs soundToString


soundFromString : String -> Maybe Sound
soundFromString =
    fromString soundPairs
