module ShowTests exposing (..)

import LongMarkdown exposing (longMarkdown)
import MarkdownNaive


--import Markdown

import Fuzz exposing (..)
import Test exposing (..)
import Expect exposing (..)
import Test.Runner.Html exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag)
import Result exposing (Result)


main : TestProgram
main =
    [ testLongString ]
        |> concat
        |> run


testLongString : Test
testLongString =
    describe "Test the time it takes to parse a long string"
        [ test "Long Markdown String" <|
            \_ ->
                Expect.true "The Parsing succeeds" <|
                    case MarkdownNaive.markdown longMarkdown of
                        Result.Ok _ ->
                            True

                        Result.Err _ ->
                            False
        ]
