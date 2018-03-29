module ShowTests exposing (..)

import Markdown exposing (toHtml)
import Fuzz exposing (..)
import Test exposing (..)
import Test.Runner.Html exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag)


main : TestProgram
main =
    [ testHeader ]
        |> concat
        |> run


testHeader : Test
testHeader =
    describe "Test generation of Markdown headers"
        [ fuzz string "# and a space followed by any string will be a header" <|
            \s ->
                toHtml ("# " ++ s)
                    |> Query.fromHtml
                    |> Query.has [ tag "h1" ]
        ]
