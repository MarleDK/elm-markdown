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
import Helpers exposing (is, isNot, whitespace, anyChar, restOfLine, indention)
import BlockToHtml exposing (processBlocks)
import Html exposing (Html)
import PreParser exposing (parse)


toHtml : String -> Html msg
toHtml s =
    s
        |> PreParser.parse
        |> PreParser.toBlocks lineToBlock
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


lineToBlock : String -> Result Error (LineBlock msg)
lineToBlock =
    run leadingWhiteSpace



-- LeadingWhiteSpaceParsers


leadingWhiteSpace : Parser (LineBlock msg)
leadingWhiteSpace =
    andThen p indention


p : Int -> Parser (LineBlock msg)
p i =
    Parser.sourceMap (\src blc -> LineBlock i blc src) block



-- Block Parsers


block : Parser (Block msg)
block =
    oneOf [ header, listItem, paragraph ]
