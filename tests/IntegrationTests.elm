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
