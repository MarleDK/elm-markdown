module Helpers exposing (..)

import Parser
    exposing
        ( Parser
        , keep
        , zeroOrMore
        , oneOrMore
        , succeed
        , repeat
        , oneOf
        , keep
        , (|=)
        , source
        , ignoreUntil
        )
import Html


-- Helpers


isNot : a -> a -> Bool
isNot a b =
    a /= b


is : a -> a -> Bool
is =
    (==)


whitespace : Char -> Bool
whitespace c =
    List.member c
        [ ' '
        , '\t'
        , '\n'
        , '\x0B'
        , '\x0C'
        ]


anyChar : Char -> Bool
anyChar =
    always True


restOfLine : Parser String
restOfLine =
    source (ignoreUntil "\n")



-- Indention


indention : Parser Int
indention =
    Parser.map List.sum
        (repeat zeroOrMore (oneOf [ tabs, spaces ]))


tabs : Parser Int
tabs =
    succeed (\x -> 4 * String.length x)
        |= keep oneOrMore (is '\t')


spaces : Parser Int
spaces =
    succeed String.length
        |= keep oneOrMore (is ' ')
