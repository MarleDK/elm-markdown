module UnitTests exposing (..)

import Blocks exposing (..)
import BlockType exposing (..)
import Test exposing (..)
import Expect
import Result exposing (Result)
import Parser


all =
    [ atxHeader, list, paragraph ]


equalTest a b c =
    test a <|
        \_ ->
            Expect.equal (Result.Ok b) (Parser.run block c)


notEqualTest a b c =
    test a <|
        \_ ->
            Expect.notEqual (Result.Ok b) (Parser.run block c)


atxHeader : Test
atxHeader =
    describe "ATXHeaders"
        [ equalTest "Base case" (ATXHeader H1 "Test") "# Test"
        , equalTest "Multiple Hashtags" (ATXHeader H4 "Test") "#### Test"
        , notEqualTest "Space is needed between # and header" (ATXHeader H4 "Test") "####Test"
        ]


list : Test
list =
    describe "Lists"
        [ equalTest "Base case" (ListItem Star "Test") "* Test"
        , notEqualTest "Space is needed between denominator and text" (ListItem Star "Test") "*Test"
        ]


paragraph : Test
paragraph =
    describe "Paragraphs"
        [ equalTest "Base case" (Paragraph "Test") "Test"
        ]
