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
    | P String


id =
    identity



-- Generally used parsers


restOfLine : Parser String
restOfLine =
    keep zeroOrMore (\c -> c /= '\n')



-- Indention handling parsers


spaceLvl0 =
    oneOf [ succeed id |. symbol " " |= spaceLvl1, s ]


spaceLvl1 =
    oneOf [ succeed id |. symbol " " |= spaceLvl2, s ]


spaceLvl2 =
    oneOf [ succeed id |. symbol " " |= spaceLvl3, s ]


spaceLvl3 =
    oneOf [ succeed id |. symbol " " |= indentionLevel1, s ]


indentionLevel1 =
    paragraph



-- FullLineConsumers


s =
    oneOf [ h, paragraph ]



-- Headers


hStrip : Char -> String -> String
hStrip c s =
    if String.isEmpty s then
        if c == '#' then
            ""
        else
            String.cons c s
    else
        String.cons c s


formatHeader s =
    s
        |> String.trim
        |> String.foldr hStrip ""
        |> String.trim


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


h =
    sourceMap headerOrText
        (succeed id |. symbol "#" |= h1)


hHelp a b =
    oneOf
        [ succeed id |. symbol "#" |= a
        , succeed id |. symbol " " |= map b restOfLine
        ]


h1 =
    hHelp h2 H1


h2 =
    hHelp h3 H2


h3 =
    hHelp h4 H3


h4 =
    hHelp h5 H4


h5 =
    hHelp h6 H5


h6 =
    hHelp nh H6


nh =
    paragraph



-- Paragraph


paragraph : Parser AST
paragraph =
    map (P) restOfLine