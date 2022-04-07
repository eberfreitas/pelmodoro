module Flash exposing (Flash, empty, new, updateFlashTime)


type alias FlashMsg =
    { time : Int
    , title : String
    , msg : String
    }


type alias Flash =
    Maybe FlashMsg


new : String -> String -> FlashMsg
new title content =
    FlashMsg 15 title content


empty : FlashMsg
empty =
    FlashMsg 0 "" ""


updateFlashTime : FlashMsg -> Maybe FlashMsg
updateFlashTime ({ time } as msg) =
    if (time - 1) < 1 then
        Nothing

    else
        Just { msg | time = time - 1 }
