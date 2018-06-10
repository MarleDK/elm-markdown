module Markdown exposing (..)

import Parser exposing (..)
import Html exposing (..)
import Result exposing (Result)


type AST
    = Document List AST
    | Header String
    | UList ListItems
    | OList ListItems
    | Paragraph String
    | BlankLine
    | CodeBlock (List String)
    | ThematicBreak


type alias ListItems =
    List String


toHtml : String -> Html msg
toHtml s =
    text (toString (markdown s))


markdown : String -> Result Error (List AST)
markdown s =
    run
        (repeat zeroOrMore
            (oneOf
                [ blankLine
                , indentedCodeBlock
                , thematicBreak
                , bulletList
                , header
                , paragraph
                ]
            )
        )
        s


blankLine : Parser AST
blankLine =
    delayedCommit (ignore zeroOrMore space) <|
        succeed BlankLine
            |. ignore (Exactly 1) newline


indentedCodeBlock : Parser AST
indentedCodeBlock =
    succeed CodeBlock
        |= repeat oneOrMore indentedCodeBlockItem


indentedCodeBlockItem : Parser String
indentedCodeBlockItem =
    succeed identity
        |. symbol "    "
        |= restOfLine
        |. lineBreak


thematicBreak : Parser AST
thematicBreak =
    flip delayedCommit (succeed ThematicBreak) <|
        spaces0To3
            |. oneOf
                [ repeat (AtLeast 3) (thematicBreakHelp "-")
                , repeat (AtLeast 3) (thematicBreakHelp "*")
                , repeat (AtLeast 3) (thematicBreakHelp "_")
                ]
            |. lineBreak


thematicBreakHelp : String -> Parser ()
thematicBreakHelp s =
    (symbol s)
        |. ignore zeroOrMore whitespace


header : Parser AST
header =
    delayedCommit spaces0To3 <|
        succeed Header
            |. symbol "# "
            |. ignore zeroOrMore space
            |= restOfLine
            |. lineBreak


bulletList : Parser AST
bulletList =
    succeed UList
        |= repeat oneOrMore bulletListItem


bulletListItem : Parser String
bulletListItem =
    succeed identity
        |. symbol "- "
        |. ignore zeroOrMore space
        |= restOfLine
        |. lineBreak


paragraph : Parser AST
paragraph =
    succeed Paragraph
        |= keep oneOrMore (not << newline)
        |. lineBreak



{-
   Here is a section for helping parsers
-}


lineBreak : Parser ()
lineBreak =
    oneOf
        [ end
        , ignore (Exactly 1) newline
        ]


restOfLine : Parser String
restOfLine =
    keep zeroOrMore (not << newline)


spaces0To3 : Parser ()
spaces0To3 =
    oneOf
        [ symbol "   "
        , symbol "  "
        , symbol " "
        , succeed ()
        ]



{-
   Here is a section of characters/character groups
-}


newline : Char -> Bool
newline c =
    c == '\n'


space : Char -> Bool
space c =
    c == ' '


whitespace : Char -> Bool
whitespace c =
    List.member c
        [ ' '
        , '\t'
        , '\n'
        , '\x0B'
        , '\x0C'
        ]
