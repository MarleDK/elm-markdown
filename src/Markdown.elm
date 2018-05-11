module AlphaTest exposing (..)

import Parser exposing (..)
import Result exposing (Result)


-- AST


type AST
    = H1 String
    | H2 String
    | H3 String
    | H4 String
    | H5 String
    | H6 String
    | L MDList
    | P String


type MDList
    = List ListItems


type ListItem
    = I String
    | L MDList



-- Helper functions


id : a -> a
id =
    identity


is : a -> a -> Bool
is c1 c2 =
    c1 == c2


isNot : a -> a -> Bool
isNot c1 c2 =
    c1 /= c2



-- Generally used parsers


restOfLine : Parser String
restOfLine =
    keep zeroOrMore (isNot '\n')



-- Indention handling parsers
{-
   Either Parse another space, and go to the next lvl or start parsing a
   part of the language.
-}


spaceLvl0 : Parser AST
spaceLvl0 =
    oneOf [ succeed id |. symbol " " |= spaceLvl1, s ]


spaceLvl1 : Parser AST
spaceLvl1 =
    oneOf [ succeed id |. symbol " " |= spaceLvl2, s ]


spaceLvl2 : Parser AST
spaceLvl2 =
    oneOf [ succeed id |. symbol " " |= spaceLvl3, s ]


spaceLvl3 : Parser AST
spaceLvl3 =
    oneOf [ succeed id |. symbol " " |= indentionLevel1, s ]


indentionLevel1 : Parser AST
indentionLevel1 =
    paragraph



-- FullLineConsumers


s =
    oneOf [ h, paragraph ]



-- Headers
-- Formatting the header text


hStrip : Char -> String -> String
hStrip c s =
    if String.isEmpty s then
        if c == '#' then
            ""
        else
            String.cons c s
    else
        String.cons c s


formatHeader : String -> String
formatHeader s =
    s
        |> String.trim
        |> String.foldr hStrip ""
        |> String.trim


headerOrText : String -> AST -> AST
headerOrText source a =
    case a of
        H1 s ->
            H1 <| formatHeader s

        H2 s ->
            H2 <| formatHeader s

        H3 s ->
            H3 <| formatHeader s

        H4 s ->
            H4 <| formatHeader s

        H5 s ->
            H5 <| formatHeader s

        H6 s ->
            H6 <| formatHeader s

        _ ->
            P source



-- Parsing of Headers


h : Parser AST
h =
    sourceMap headerOrText
        (succeed id |. symbol "#" |= h1)


hHelp : Parser b -> (String -> b) -> Parser b
hHelp a b =
    oneOf
        [ succeed id |. symbol "#" |= a
        , succeed id |. symbol " " |= map b restOfLine
        ]


h1 : Parser AST
h1 =
    hHelp h2 H1


h2 : Parser AST
h2 =
    hHelp h3 H2


h3 : Parser AST
h3 =
    hHelp h4 H3


h4 : Parser AST
h4 =
    hHelp h5 H4


h5 : Parser AST
h5 =
    hHelp h6 H5


h6 : Parser AST
h6 =
    hHelp nh H6


nh : Parser AST
nh =
    paragraph



-- Lists
{-
   Lists can either be bullet lists, with one of the following markers:
   -, + or *
   Or an ordered list consisting of digits followed by either `.` or `)`
-}


listItem : (Char -> Bool) -> Parser Item
listItem c =
    ignore c
        |= restOfLine


bulletList c ind =
    ignore


bulletList1 : Parser AST
bulletList1 =
    bulletList (is '-')


bulletList2 : Parser AST
bulletList2 =
    bulletList (is '+')


bulletList3 : Parser AST
bulletList3 =
    bulletList (is '*')



-- Paragraph


paragraph : Parser AST
paragraph =
    map (P) restOfLine
