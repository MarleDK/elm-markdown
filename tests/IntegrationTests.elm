module IntegrationTests exposing (..)

import Test exposing (..)
import Expect
import Test.Runner.Html as Runner
import Result exposing (Result)
import Markdown
import Html


all : List Test
all =
    [ html
    , atxHeader
    ]


htmlMarkdownInput1 : String
htmlMarkdownInput1 =
    """<pre>
hej
</pre>
# Hej"""


htmlMarkdownOutput1 : Html.Html msg
htmlMarkdownOutput1 =
    Html.div []
        [ Html.pre []
            [ Html.text "\nhej\n" ]
        , Html.h1 [] [ Html.text "Hej" ]
        ]


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


toTest : ( String, String, Html.Html msg ) -> Test
toTest ( desc, input, output ) =
    test desc <|
        \_ ->
            Expect.equal output
                (Markdown.toHtml input)


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
      , "   Test      "
      , Html.div []
            [ Html.p [] [ Html.text "Test" ]
            ]
      )
    ]


paragraph : Test
paragraph =
    describe "Paragraphs" (List.map toTest paragraphs)
