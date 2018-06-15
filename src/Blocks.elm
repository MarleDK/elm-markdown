module Blocks exposing (block)

import Parser exposing (Parser, oneOf, succeed, symbol, (|.), (|=))
import Helpers exposing (restOfLine)
import BlockType exposing (Block(..), Denominator(..), HeaderLevel(..))


block : Parser (Block msg)
block =
    oneOf [ header, listItem, paragraph ]



-- ATXHeaders


header : Parser (Block msg)
header =
    oneOf
        [ succeed (ATXHeader H1)
            |. symbol "# "
            |= restOfLine
        , succeed (ATXHeader H2)
            |. symbol "## "
            |= restOfLine
        , succeed (ATXHeader H3)
            |. symbol "### "
            |= restOfLine
        , succeed (ATXHeader H4)
            |. symbol "#### "
            |= restOfLine
        , succeed (ATXHeader H5)
            |. symbol "##### "
            |= restOfLine
        , succeed (ATXHeader H6)
            |. symbol "###### "
            |= restOfLine
        ]



-- Paragraph


paragraph : Parser (Block msg)
paragraph =
    Parser.map (Paragraph) restOfLine



-- Lists
{-
   Lists can either be bullet lists, with one of the following markers:
   -, + or *
   Or an ordered list consisting of digits followed by either `.` or `)`
-}


listItem : Parser (Block msg)
listItem =
    oneOf [ bulletList1, bulletList2, bulletList3 ]


bulletItemHelper : String -> Denominator -> Parser (Block msg)
bulletItemHelper c d =
    succeed (ListItem d)
        |. symbol c
        |= restOfLine


bulletList1 : Parser (Block msg)
bulletList1 =
    bulletItemHelper "- " Minus


bulletList2 : Parser (Block msg)
bulletList2 =
    bulletItemHelper "+ " Plus


bulletList3 : Parser (Block msg)
bulletList3 =
    bulletItemHelper "* " Star
