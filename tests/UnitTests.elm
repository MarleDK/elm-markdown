module ShowTests exposing (..)

import Blocks exposing (..)
import BlockType exposing (..)
import Test exposing (..)
import Expect
import Test.Runner.Html as Runner
import Result exposing (Result)
import Parser
import IntegrationTests


main : Runner.TestProgram
main =
    [ atxHeader, list ]
        |> List.append IntegrationTests.all
        |> concat
        |> Runner.run


atxHeader : Test
atxHeader =
    describe "ATXHeaders"
        [ test "Base case" <|
            \_ ->
                Expect.equal (Result.Ok (ATXHeader H1 "Test1")) <|
                    Parser.run header "# Test1"
        , test "Multiple Hashtags" <|
            \_ ->
                Expect.equal (Result.Ok (ATXHeader H4 "Test2")) <|
                    Parser.run header "#### Test2"
        , test "Multiple spaces after hash are not removed yet" <|
            \_ ->
                Expect.equal (Result.Ok (ATXHeader H2 "   Test3")) <|
                    Parser.run header "##    Test3"
        ]


list : Test
list =
    describe "Lists"
        [ test "Base case" <|
            \_ ->
                Expect.equal (Result.Ok (ListItem Star "Test4")) <|
                    Parser.run listItem "* Test4"
        ]
