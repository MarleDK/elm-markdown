module Markdown2 exposing (..)

import Parser exposing (..)
import Html exposing (..)
import Result exposing (Result)
import String


type AST
    = Document List AST
    | Header String
    | UList ListItems
    | OList ListItems
    | Paragraph String


type alias ListItems =
    List String


toHtml : String -> Html msg
toHtml s =
    text (toString (markdown s))


markdown : String -> List (Result Error AST)
markdown str =
    String.split "\n" str
        |> List.map parse



{-
   proccessLines : List String -> AST -> List (Result Error AST)
   proccessLines ast str =
       case parse str of
           Ok ast ->
-}


parse : String -> Result Error AST
parse s =
    run
        (oneOf
            [ header
            , normal
            ]
        )
        s


header : Parser AST
header =
    delayedCommit (symbol "#") <|
        succeed Header
            |. ignore oneOrMore (\c -> c == ' ')
            |= keep zeroOrMore (\c -> c /= '\n')
            |. ignore zeroOrMore (\c -> c /= '\n')


bulletListItem : Parser String
bulletListItem =
    succeed identity
        |. symbol "-"
        |. ignore oneOrMore (\c -> c == ' ')
        |= keep zeroOrMore (\c -> c /= '\n')
        |. ignore zeroOrMore (\c -> c /= '\n')


normal : Parser AST
normal =
    succeed Paragraph
        |= keep zeroOrMore (\c -> c /= '\n')
        |. ignore zeroOrMore (\c -> c /= '\n')
