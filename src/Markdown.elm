module Markdown exposing (..)

import Parser
    exposing
        ( Error
        , Parser
        , Count(..)
        , run
        , oneOf
        , delayedCommit
        , succeed
        , ignore
        , keep
        , oneOrMore
        , zeroOrMore
        , repeat
        , symbol
        , end
        , (|.)
        , (|=)
        )
import Html exposing (..)
import Result exposing (Result)
import Result.Extra exposing (combine)
import String


type Block
    = Document Blocks
    | List Blocks
    | ListItem ListType Blocks


type ListType
    = BulletList ListSpacing
    | OrderedList ListSpacing


type ListSpacing
    = Tight
    | Loose


type alias Blocks =
    List Block


toHtml : String -> Html msg
toHtml s =
    text (toString (markdown s))


markdown : String -> Result Error (List AST)
markdown str =
    String.lines str
        |> List.foldl proccessLine
        |> combine


proccessLine : String -> Result Error AST
proccessLine str =
    parse str


parse : String -> Result Error AST
parse s =
    run
        (oneOf
            [ blankLine
            , indentedCodeBlock
            , thematicBreak
            , bulletList
            , header
            , paragraph
            ]
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


thematicBreak : Parser AST
thematicBreak =
    flip delayedCommit (succeed ThematicBreak) <|
        spaces0To3
            |. oneOf
                [ repeat (AtLeast 3) (thematicBreakHelp "-")
                , repeat (AtLeast 3) (thematicBreakHelp "*")
                , repeat (AtLeast 3) (thematicBreakHelp "_")
                ]
            |. end


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


paragraph : Parser AST
paragraph =
    succeed Paragraph
        |= restOfLine



{-
   Here is a section for helping parsers
-}


restOfLine : Parser String
restOfLine =
    keep zeroOrMore (\_ -> True)


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
