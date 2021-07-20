module Tools exposing
    ( continuityDisplayPairs
    , continuityFromDisplay
    , continuityFromString
    , continuityToString
    , fromString
    , makePairs
    , notificationsDefault
    , sentimentFromString
    , sentimentToDisplay
    , sentimentToString
    , soundDisplayPairs
    , soundFromDisplay
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


continuityToDisplay : Continuity -> String
continuityToDisplay continuity =
    case continuity of
        NoCont ->
            "No continuity"

        SimpleCont ->
            "Simple continuity"

        FullCont ->
            "Full continuity"


continuityList : List Continuity
continuityList =
    [ NoCont
    , SimpleCont
    , FullCont
    ]


continuityPairs : List ( Continuity, String )
continuityPairs =
    continuityList |> makePairs continuityToString


continuityDisplayPairs : List ( Continuity, String )
continuityDisplayPairs =
    continuityList |> makePairs continuityToDisplay


continuityFromString : String -> Maybe Continuity
continuityFromString =
    fromString continuityPairs


continuityFromDisplay : String -> Maybe Continuity
continuityFromDisplay =
    fromString continuityDisplayPairs


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

        Bell ->
            "bell"

        AlarmClock ->
            "alarm-clock"

        Bong ->
            "bong"

        RelaxingPercussion ->
            "relaxing-percussion"

        BirdSong ->
            "bird-song"


soundToDisplay : Sound -> String
soundToDisplay sound =
    case sound of
        WindChimes ->
            "Wind Chimes"

        Bell ->
            "Bell"

        AlarmClock ->
            "Alarm Clock"

        Bong ->
            "Bong"

        RelaxingPercussion ->
            "Relaxing Percussion"

        BirdSong ->
            "Bird Song"


soundList : List Sound
soundList =
    [ WindChimes
    , Bell
    , AlarmClock
    , Bong
    , RelaxingPercussion
    , BirdSong
    ]


soundPairs : List ( Sound, String )
soundPairs =
    soundList |> makePairs soundToString


soundDisplayPairs : List ( Sound, String )
soundDisplayPairs =
    soundList |> makePairs soundToDisplay


soundFromString : String -> Maybe Sound
soundFromString =
    fromString soundPairs


soundFromDisplay : String -> Maybe Sound
soundFromDisplay =
    fromString soundDisplayPairs
