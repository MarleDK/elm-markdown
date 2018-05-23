module Markdown exposing (..)

import Parser
    exposing
        ( Error
        , Parser
        , Count(..)
        , run
        , oneOf
        , delayedCommit
        , succeed
        , ignore
        , keep
        , oneOrMore
        , zeroOrMore
        , repeat
        , symbol
        , end
        , (|.)
        , (|=)
        , andThen
        )
import Result exposing (Result(..))
import Result.Extra exposing (combine)
import String
import Blocks exposing (header, listItem, paragraph)
import BlockType exposing (LineBlock, LineBlocks, Block, Blocks, Denominator)
import Helpers exposing (is, isNot, whitespace, anyChar, restOfLine)
import BlockToHtml exposing (processBlocks)
import Html exposing (Html)


toHtml : String -> Html msg
toHtml s =
    String.lines s
        |> List.map lineToBlock
        |> combine
        |> Result.map processBlocks
        |> Result.Extra.extract
            (\x ->
                Html.div []
                    [ Html.p [] [ Html.text s ]
                    , Html.p [] []
                    , Html.p [] [ Html.text (toString x) ]
                    ]
            )


lineToBlock : String -> Result Error LineBlock
lineToBlock =
    run leadingWhiteSpace



-- LeadingWhiteSpaceParsers


leadingWhiteSpace : Parser LineBlock
leadingWhiteSpace =
    andThen p indention


p : Int -> Parser LineBlock
p i =
    Parser.sourceMap (\src blc -> LineBlock i blc src) block


indention : Parser Int
indention =
    Parser.map List.sum
        (repeat zeroOrMore (oneOf [ tabs, spaces ]))


tabs : Parser Int
tabs =
    succeed (\x -> 4 * String.length x)
        |= keep oneOrMore (is '\t')


spaces : Parser Int
spaces =
    succeed String.length
        |= keep oneOrMore (is ' ')



-- Block Parsers


block : Parser Block
block =
    oneOf [ header, listItem, paragraph ]
