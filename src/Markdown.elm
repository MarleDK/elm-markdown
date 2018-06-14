module Markdown exposing (..)

import Parser
    exposing
        ( Error
        , Parser
        , Count(..)
        , run
        , oneOf
        , succeed
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
import Result.Extra
import String
import Blocks exposing (block)
import BlockType
    exposing
        ( LineBlock
        , LineBlocks
        , Block
        , Denominator
        )
import Helpers
    exposing
        ( is
        , isNot
        , whitespace
        , anyChar
        , restOfLine
        , indention
        )
import BlockToHtml exposing (processBlocks)
import Html exposing (Html)
import PreParser exposing (parse)


toHtml : String -> Html msg
toHtml s =
    s
        |> PreParser.parse
        |> PreParser.toBlocks lineToBlock
        |> Result.Extra.combine
        |> Result.map processBlocks
        |> Result.Extra.extract (extractError s)


extractError : String -> Error -> Html msg
extractError s err =
    Html.div []
        [ Html.p [] [ Html.text s ]
        , Html.p [] []
        , Html.p [] [ Html.text (toString err) ]
        ]


lineToBlock : String -> Result Error (LineBlock msg)
lineToBlock =
    run parseLine



{- parseLine parses the indentation, and gives an integer of the
   indention level as input to parseLineBlock.
-}


parseLine : Parser (LineBlock msg)
parseLine =
    andThen parseLineBlock indention



{- Parser.sourceMap is a function, which takes a function:
   (string -> a -> b), and a (Parser a) as inputs, and returns a
   (Parser b). Here it it used to save the source string parsed, if it
   later on is discovered, that the source should be used.
-}


parseLineBlock : Int -> Parser (LineBlock msg)
parseLineBlock indention =
    Parser.sourceMap (\src blc -> LineBlock indention blc src) block
