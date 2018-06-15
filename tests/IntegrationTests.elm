module IntegrationTests exposing (..)

import Test exposing (..)
import Expect
import Test.Runner.Html as Runner
import Result exposing (Result)
import Markdown
import Html
import LongMarkdown
import Test.Html.Selector as Selector
import Test.Html.Query as Query


all : List Test
all =
    [ html
    , atxHeader
    , paragraph
    , longMarkdown
    , list
    ]


toTest : ( String, String, Html.Html msg ) -> Test
toTest ( desc, input, output ) =
    test desc <|
        \_ ->
            Expect.equal output
                (Markdown.toHtml input)


htmlIOTests : List ( String, String, Html.Html msg )
htmlIOTests =
    [ ( "Base case 1 - HTML Blocks are handled - according to rule 1"
      , "<pre>\nhej\n</pre>\n# Hej"
      , Html.div []
            [ Html.pre []
                [ Html.text "\nhej\n" ]
            , Html.h1 [] [ Html.text "Hej" ]
            ]
      )
    , ( "Base case 2 - HTML Blocks are handled - according to rule 2"
      , "# Test\n<!-- Not Shown -->Shown\nShown after"
      , Html.div []
            [ Html.h1 [] [ Html.text "Test" ]
            , Html.text ""
            , Html.text "Shown"
            , Html.p [] [ Html.text "Shown after" ]
            ]
      )
    ]


html : Test
html =
    describe "HTML Blocks" (List.map toTest htmlIOTests)


atxHeaders : List ( String, String, Html.Html msg )
atxHeaders =
    [ ( "Base case"
      , "# Test"
      , Html.div []
            [ Html.h1 [] [ Html.text "Test" ] ]
      )
    ]


atxHeader : Test
atxHeader =
    describe "ATX Headers" (List.map toTest atxHeaders)


paragraphs : List ( String, String, Html.Html msg )
paragraphs =
    [ ( "Base case"
      , "Test"
      , Html.div []
            [ Html.p [] [ Html.text "Test" ]
            ]
      )
    , ( "Spaces are stripped"
      , "   Test"
      , Html.div []
            [ Html.p [] [ Html.text "Test" ]
            ]
      )
    ]


paragraph : Test
paragraph =
    describe "Paragraphs Integration tests" (List.map toTest paragraphs)


longMarkdown : Test
longMarkdown =
    describe "Test that the long markdown does not fail when parsing"
        [ test "Long markdown does not fail" <|
            \_ ->
                (Markdown.toHtml LongMarkdown.longMarkdown)
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.text "Error" ]
        ]


lists : List ( String, String, Html.Html msg )
lists =
    [ ( "Base case"
      , "- Test"
      , Html.div []
            [ Html.ul []
                [ Html.li [] [ Html.text "Test" ] ]
            ]
      )
    , ( "Multiple items"
      , "- Test\n- NewTest"
      , Html.div []
            [ Html.ul []
                [ Html.li [] [ Html.text "Test" ]
                , Html.li [] [ Html.text "NewTest" ]
                ]
            ]
      )
    , ( "Nested items"
      , "- Test\n  - NestedItem\n- NonNested"
      , Html.div []
            [ Html.ul []
                [ Html.li [] [ Html.text "Test" ]
                , Html.ul [] [ Html.li [] [ Html.text "NestedItem" ] ]
                , Html.li [] [ Html.text "NonNested" ]
                ]
            ]
      )
    ]


list : Test
list =
    describe "Integration test of Lists" (List.map toTest lists)
