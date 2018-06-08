module PreParser exposing (parse, toBlocks)

import Result exposing (Result(..))
import Parser
    exposing
        ( Parser
        , run
        , succeed
        , (|.)
        , (|=)
        , symbol
        , oneOf
        , repeat
        , ignore
        , ignoreUntil
        , zeroOrMore
        , oneOrMore
        , end
        , delayedCommit
        , Error
        )
import Helpers exposing (restOfLine, indention, whitespace, isNot, is)
import Char
import HtmlParser
import HtmlParser.Util
import BlockType exposing (LineBlock, Block(..))


type Line a
    = S a
    | H String


toBlocks : (a -> Result Error (LineBlock msg)) -> List (Line a) -> List (Result Error (LineBlock msg))
toBlocks f x =
    List.concatMap (toBlock f) x


toBlock : (a -> Result Error (LineBlock msg)) -> Line a -> List (Result Error (LineBlock msg))
toBlock f x =
    case x of
        S a ->
            [ f a ]

        H y ->
            y
                |> HtmlParser.parse
                |> HtmlParser.Util.toVirtualDom
                |> List.map HtmlBlock
                |> List.map (flip (LineBlock 0) y)
                |> List.map Result.Ok


combineHs : List (Line String) -> List (Line String)
combineHs l =
    case l of
        [] ->
            []

        (H x) :: (H y) :: t ->
            combineHs (H (x ++ "\n" ++ y) :: t)

        (H x) :: t ->
            (H x) :: combineHs t

        (S x) :: t ->
            (S x) :: combineHs t


parse : String -> List (Line String)
parse s =
    s
        |> String.lines
        |> takeS
        |> combineHs


takeS : List String -> List (Line String)
takeS lines =
    case lines of
        [] ->
            []

        h :: t ->
            case run parser h of
                Ok endCond ->
                    takeH lines endCond

                Err _ ->
                    (S h) :: (takeS t)


takeH : List String -> (String -> Bool) -> List (Line String)
takeH lines endCond =
    case lines of
        [] ->
            []

        h :: t ->
            case endCond h of
                True ->
                    (H h) :: (takeS t)

                False ->
                    (H h) :: (takeH t endCond)


parser : Parser (String -> Bool)
parser =
    Parser.andThen html indention


html : Int -> Parser (String -> Bool)
html i =
    if i < 4 then
        oneOf
            [ html1
            , html3
            , html24And5
            , html7
            ]
    else
        Parser.fail "Too much indention"


html1EndCond : String -> Bool
html1EndCond s =
    String.contains "</script>" s
        || String.contains "</style>" s
        || String.contains "</pre>" s


html1 : Parser (String -> Bool)
html1 =
    succeed html1EndCond
        |. oneOf
            [ symbol "<script"
            , symbol "<pre"
            , symbol "<style"
            ]
        |. ignore zeroOrMore whitespace
        |. oneOf
            [ symbol ">"
            , end
            ]


html3EndCond : String -> Bool
html3EndCond =
    String.contains "?>"


html3 : Parser (String -> Bool)
html3 =
    succeed html3EndCond
        |. symbol "<?"


htmlCommentEndCond : String -> Bool
htmlCommentEndCond =
    String.contains "-->"


html4EndCond : String -> Bool
html4EndCond =
    String.contains ">"


html5EndCond : String -> Bool
html5EndCond =
    String.contains "]]>"


html24And5 : Parser (String -> Bool)
html24And5 =
    succeed identity
        |. symbol "<!"
        |= oneOf
            [ succeed htmlCommentEndCond |. symbol "--"
            , succeed html4EndCond |. ignore oneOrMore Char.isUpper
            , succeed html5EndCond |. symbol "[CDATA["
            ]


blankLine : String -> Bool
blankLine s =
    s
        |> String.trim
        |> String.isEmpty


html7 : Parser (String -> Bool)
html7 =
    succeed blankLine
        |. symbol "<"
        |. oneOf
            [ openTag
            , closingTag
            ]


openTag : Parser ()
openTag =
    tagName
        |. repeat zeroOrMore maybeAttribute
        |. ignore zeroOrMore whitespace
        |. oneOf
            [ symbol "/>"
            , symbol ">"
            ]


closingTag : Parser ()
closingTag =
    symbol "/"
        |. tagName
        |. ignore zeroOrMore whitespace
        |. symbol ">"


tagName : Parser ()
tagName =
    letter
        |. repeat zeroOrMore
            (oneOf
                [ letter
                , digit
                , symbol "-"
                ]
            )


maybeAttribute : Parser ()
maybeAttribute =
    delayedCommit (ignore oneOrMore whitespace) <|
        attributeName
            |. oneOf
                [ attributeValue
                , succeed ()
                ]


attributeName : Parser ()
attributeName =
    oneOf
        [ letter
        , symbol "_"
        , symbol ":"
        ]
        |. repeat zeroOrMore
            (oneOf
                [ letter
                , digit
                , symbol "_"
                , symbol ":"
                , symbol "."
                , symbol "-"
                ]
            )


attributeValue : Parser ()
attributeValue =
    oneOf
        [ symbol "'" |. ignore zeroOrMore (isNot '\'') |. symbol "'"
        , symbol "\"" |. ignore zeroOrMore (isNot '"') |. symbol "\""
        , ignore oneOrMore
            (\x ->
                not <|
                    List.any
                        (is x)
                        [ '"'
                        , '\''
                        , '='
                        , '<'
                        , '>'
                        , '`'
                        ]
            )
        ]


letter : Parser ()
letter =
    oneOf
        [ symbol "a"
        , symbol "b"
        , symbol "c"
        , symbol "d"
        , symbol "e"
        , symbol "f"
        , symbol "g"
        , symbol "h"
        , symbol "i"
        , symbol "j"
        , symbol "k"
        , symbol "l"
        , symbol "m"
        , symbol "n"
        , symbol "o"
        , symbol "p"
        , symbol "q"
        , symbol "r"
        , symbol "s"
        , symbol "t"
        , symbol "u"
        , symbol "v"
        , symbol "w"
        , symbol "x"
        , symbol "y"
        , symbol "z"
        , symbol "A"
        , symbol "B"
        , symbol "C"
        , symbol "D"
        , symbol "E"
        , symbol "F"
        , symbol "G"
        , symbol "H"
        , symbol "I"
        , symbol "J"
        , symbol "K"
        , symbol "L"
        , symbol "M"
        , symbol "N"
        , symbol "O"
        , symbol "P"
        , symbol "Q"
        , symbol "R"
        , symbol "S"
        , symbol "T"
        , symbol "U"
        , symbol "V"
        , symbol "W"
        , symbol "X"
        , symbol "Y"
        , symbol "Z"
        ]


digit : Parser ()
digit =
    oneOf
        [ symbol "1"
        , symbol "2"
        , symbol "3"
        , symbol "4"
        , symbol "5"
        , symbol "6"
        , symbol "7"
        , symbol "8"
        , symbol "9"
        ]