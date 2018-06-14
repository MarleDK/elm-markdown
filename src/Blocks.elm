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
    (succeed identity |. symbol "#" |= h1)


hHelp : Parser (Block msg) -> (String -> Block msg) -> Parser (Block msg)
hHelp a b =
    oneOf
        [ succeed identity
            |. symbol "#"
            |= a
        , succeed identity
            |. symbol " "
            |= Parser.map b restOfLine
        , paragraph
        ]


h1 : Parser (Block msg)
h1 =
    hHelp h2 (ATXHeader H1)


h2 : Parser (Block msg)
h2 =
    hHelp h3 (ATXHeader H2)


h3 : Parser (Block msg)
h3 =
    hHelp h4 (ATXHeader H3)


h4 : Parser (Block msg)
h4 =
    hHelp h5 (ATXHeader H4)


h5 : Parser (Block msg)
h5 =
    hHelp h6 (ATXHeader H5)


h6 : Parser (Block msg)
h6 =
    hHelp nh (ATXHeader H6)


nh : Parser (Block msg)
nh =
    paragraph



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
    succeed identity
        |. symbol c
        |= oneOf
            [ succeed (ListItem d)
                |. symbol " "
                |= restOfLine
            , paragraph
            ]


bulletList1 : Parser (Block msg)
bulletList1 =
    bulletItemHelper "-" Minus


bulletList2 : Parser (Block msg)
bulletList2 =
    bulletItemHelper "+" Plus


bulletList3 : Parser (Block msg)
bulletList3 =
    bulletItemHelper "*" Star
