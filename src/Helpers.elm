module Helpers exposing (..)

import Parser exposing (Parser, keep, zeroOrMore)
import Html


-- Helpers


isNot : a -> a -> Bool
isNot a b =
    not (is a b)


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
    keep zeroOrMore anyChar
