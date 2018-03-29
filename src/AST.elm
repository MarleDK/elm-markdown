module AST exposing(AST)

type AST = Header String
    | UList ListItems
    | OList ListItems

type ListItems = List String