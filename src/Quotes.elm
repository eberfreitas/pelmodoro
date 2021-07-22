module Quotes exposing (randomHtmlQuote)

import Html.Styled as Html
import Misc
import Random
import Time


type alias Quote =
    { quote : String
    , author : String
    }


allQuotes : List Quote
allQuotes =
    [ Quote "The future is always beginning now." "Mark Strand"
    , Quote "In today's rush, we all think too much - seek too much - want too much - and forget about the joy of just being." "Eckhart Tolle"
    , Quote "Few of us ever life in the present. We are forever anticipating what is to come or remembering what has gone." "Louis L'Amour"
    , Quote "The real question is not whether life exists after death. The real question is whether you are alive before death." "Osho"
    , Quote "Paradise is not a place; it's a state of consciousness." "Sri Chinmoy"
    , Quote "Each morning we are born again. What we do today is what matters most." "Buddha"
    , Quote "Do not dwell in the past, do not dream of the future, concentrate the mind on the present moment." "Buddha"
    , Quote "He who has freed himself of the disease of 'tomorrow' has a chance to attain what he came here for." "G.I. Gurdjieff"
    , Quote "Your vision will become clear only when you can look into your own heart. Who looks outside, dreams; who looks inside, awakes." "Carl Jung"
    , Quote "The little things? The little moments? They aren’t little." "Jon Kabat-Zinn"
    , Quote "To think in terms of either pessimism or optimism oversimplifies the truth. The problem is to see reality as it is." "Thích Nhất Hạnh"
    , Quote "When we get too caught up in the busyness of the world, we lose connection with one another – and ourselves." "Jack Kornfield"
    , Quote "Wisdom says we are nothing. Love says we are everything. Between these two our life flows." "Jack Kornfield"
    , Quote "You cannot control the results, only your actions." "Allan Lokos"
    , Quote "Don’t believe everything you think. Thoughts are just that – thoughts." "Allan Lokos"
    , Quote "Step outside for a while – calm your mind. It is better to hug a tree than to bang your head against a wall continually." "Rasheed Ogunlaru"
    , Quote "How you look at it is pretty much how you’ll see it." "Rasheed Ogunlaru"
    , Quote "Treat everyone you meet as if they were you." "Doug Dillon"
    , Quote "The mind is just like a muscle – the more you exercise it, the stronger it gets and the more it can expand." "Idowu Koyenikan"
    , Quote "The only way to live is by accepting each minute as an unrepeatable miracle." "Tara Brach"
    ]


pickQuote : Time.Posix -> List Quote -> Maybe Quote
pickQuote time quotes =
    case quotes of
        head :: tail ->
            tail
                |> Random.uniform head
                |> Misc.flip Random.step (time |> Time.posixToMillis |> Random.initialSeed)
                |> Tuple.first
                |> Just

        _ ->
            Nothing


quoteToHtml : Quote -> Html.Html msg
quoteToHtml quote =
    Html.div []
        [ Html.strong [] [ Html.text quote.quote ]
        , Html.text " — "
        , Html.i [] [ Html.text quote.author ]
        ]


randomHtmlQuote : Time.Posix -> Html.Html msg
randomHtmlQuote time =
    allQuotes
        |> pickQuote time
        |> Maybe.map quoteToHtml
        |> Maybe.withDefault (Html.text "")
