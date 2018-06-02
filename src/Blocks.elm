module Blocks
    exposing
        ( header
        , paragraph
        , listItem
        )

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
        , andThen
        )
import Helpers exposing (is, isNot, whitespace, anyChar, restOfLine)
import BlockType
    exposing
        ( LineBlock
        , LineBlocks
        , Block(..)
        , Blocks
        , Denominator(..)
        , HeaderLevel(..)
        )


-- ATXHeaders


header : Parser Block
header =
    (succeed identity |. symbol "#" |= h1)


hHelp : Parser Block -> (String -> Block) -> Parser Block
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


h1 : Parser Block
h1 =
    hHelp h2 (ATXHeader H1)


h2 : Parser Block
h2 =
    hHelp h3 (ATXHeader H2)


h3 : Parser Block
h3 =
    hHelp h4 (ATXHeader H3)


h4 : Parser Block
h4 =
    hHelp h5 (ATXHeader H4)


h5 : Parser Block
h5 =
    hHelp h6 (ATXHeader H5)


h6 : Parser Block
h6 =
    hHelp nh (ATXHeader H6)


nh : Parser Block
nh =
    paragraph



-- Paragraph


paragraph : Parser Block
paragraph =
    Parser.map (Paragraph) restOfLine



-- Lists
{-
   Lists can either be bullet lists, with one of the following markers:
   -, + or *
   Or an ordered list consisting of digits followed by either `.` or `)`
-}


listItem : Parser Block
listItem =
    oneOf [ bulletList1, bulletList2, bulletList3 ]


bulletItemHelper : String -> Denominator -> Parser Block
bulletItemHelper c d =
    succeed identity
        |. symbol c
        |= oneOf
            [ succeed (ListItem d)
                |. symbol " "
                |= restOfLine
            , paragraph
            ]


bulletList1 : Parser Block
bulletList1 =
    bulletItemHelper "-" Minus


bulletList2 : Parser Block
bulletList2 =
    bulletItemHelper "+" Plus


bulletList3 : Parser Block
bulletList3 =
    bulletItemHelper "*" Star



{-
   thematicBreak : Parser Block
   thematicBreak =


   thematicBreakHelp : String -> Parser ()
   thematicBreakHelp s =
       (symbol s)
           |. ignore zeroOrMore whitespace



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
-}