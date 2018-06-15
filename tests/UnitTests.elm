module UnitTests exposing (..)

import Blocks exposing (..)
import BlockType exposing (..)
import Test exposing (..)
import Fuzz exposing (Fuzzer, string, oneOf, constant, list)
import Expect exposing (Expectation)
import Result exposing (Result(..))
import Parser exposing (Parser)


all : List Test
all =
    [ atxHeader, list, paragraph, blockFuzzTest ]


equalTest : String -> Block msg -> String -> Test
equalTest a b c =
    test a <|
        \_ ->
            Expect.equal (Result.Ok b) (Parser.run block c)


notEqualTest : String -> Block msg -> String -> Test
notEqualTest a b c =
    test a <|
        \_ ->
            Expect.notEqual (Result.Ok b) (Parser.run block c)


atxHeader : Test
atxHeader =
    describe "ATXHeaders"
        [ equalTest "Base case" (ATXHeader H1 "Test") "# Test"
        , equalTest "Multiple Hashtags"
            (ATXHeader H4 "Test")
            "#### Test"
        , notEqualTest "Space is needed between # and header"
            (ATXHeader H4 "Test")
            "####Test"
        ]


list : Test
list =
    describe "Lists"
        [ equalTest "Base case" (ListItem Star "Test") "* Test"
        , notEqualTest "Space is needed between denominator and text"
            (ListItem Star "Test")
            "*Test"
        ]


paragraph : Test
paragraph =
    describe "Paragraphs"
        [ equalTest "Base case" (Paragraph "Test") "Test"
        ]


successfullResult : Result a e -> Expectation
successfullResult parser =
    case parser of
        Ok _ ->
            Expect.pass

        Err e ->
            Expect.fail (toString e)


blockFuzzTest : Test
blockFuzzTest =
    describe "This is a fuzz test of Blocks.block"
        [ fuzz string "Every string parses successfully" <|
            Parser.run block
                >> successfullResult
        , fuzz specialChars "Any String of special chars are valid" <|
            List.map String.fromChar
                >> String.concat
                >> Parser.run block
                >> successfullResult
        , fuzz specialString "Strings starting with special char are valid" <|
            Parser.run block
                >> successfullResult
        ]


specialChars : Fuzzer (List Char)
specialChars =
    Fuzz.list specialChar


specialChar : Fuzzer Char
specialChar =
    oneOf
        [ constant '\n'
        , constant '#'
        , constant '-'
        , constant '+'
        , constant '*'
        , constant ' '
        ]


specialString : Fuzzer String
specialString =
    Fuzz.map2 (\c s -> String.cons c s) specialChar string
