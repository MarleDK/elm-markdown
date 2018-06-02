module BlockType
    exposing
        ( LineBlock
        , LineBlocks
        , Block(..)
        , Blocks
        , Denominator(..)
        , HeaderLevel(..)
        , hlToHTML
        , blockToHtml
        )

-- Imports

import Html exposing (..)


-- Type Definitions


type alias LineBlock msg =
    { indent : Int
    , block : Block msg
    , source : String
    }


type alias LineBlocks msg =
    List (LineBlock msg)


type Block msg
    = ATXHeader HeaderLevel String
    | Paragraph String
    | ListItem Denominator String
    | HtmlBlock (Html msg)


type alias Blocks msg =
    List (Block msg)



{- | Denominator
   Minus:       -
   Plus:        +
   Star:        *
   Number(eg.): 1.
-}


type Denominator
    = Minus
    | Plus
    | Star
    | Number



-- HeaderLevel


type HeaderLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


blockToHtml : Block msg -> Html msg
blockToHtml block =
    case block of
        ATXHeader hl s ->
            (hlToHTML hl) [] [ text s ]

        Paragraph s ->
            p [] [ text s ]

        ListItem _ s ->
            li [] [ text s ]

        HtmlBlock x ->
            x


hlToHTML : HeaderLevel -> (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg)
hlToHTML hl =
    case hl of
        H1 ->
            h1

        H2 ->
            h2

        H3 ->
            h3

        H4 ->
            h4

        H5 ->
            h5

        H6 ->
            h6
