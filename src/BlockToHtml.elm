module BlockToHtml exposing (processBlocks)

import BlockType
    exposing
        ( Denominator(..)
        , LineBlock
        , LineBlocks
        , Block(..)
        , hlToHTML
        , blockToHtml
        )
import Html exposing (..)
import Tuple


{-
   This module is responsible for transforming a list of Blocks to
   the right HTML representation.
   This requires some context when processing the list.
-}


processBlocks : LineBlocks msg -> Html msg
processBlocks blocks =
    div [] (processBlocksHelp blocks)


processBlocksHelp : LineBlocks msg -> List (Html msg)
processBlocksHelp blocks =
    case blocks of
        [] ->
            []

        h :: t ->
            processNonEmptyBlocksHelp h t


processNonEmptyBlocksHelp :
    LineBlock msg
    -> LineBlocks msg
    -> List (Html msg)
processNonEmptyBlocksHelp h t =
    if h.indent < 4 then
        case h.block of
            ATXHeader hl s ->
                ((hlToHTML hl) [] [ text s ]) :: (processBlocksHelp t)

            ListItem denom s ->
                let
                    ( html, rest ) =
                        newList h.indent denom s t
                in
                    html :: processBlocksHelp rest

            Paragraph s ->
                (p [] [ text s ])
                    :: (processBlocksHelp t)

            HtmlBlock x ->
                x :: processBlocksHelp t
    else
        let
            ( codeBlocks, rest ) =
                proccessIndCodeBlock (h :: t)

            codeBlock =
                String.join "\n" codeBlocks
        in
            (pre [] [ code [] [ text codeBlock ] ])
                :: processBlocksHelp rest



-- Lists


newList :
    Int
    -> Denominator
    -> String
    -> LineBlocks msg
    -> ( Html msg, LineBlocks msg )
newList x denom s t =
    let
        ( html, inItem, restOfBlocks ) =
            proccessList x denom t

        listItems =
            (li [] (text s :: inItem)) :: html
    in
        ( (ul [] listItems), restOfBlocks )


proccessList :
    Int
    -> Denominator
    -> LineBlocks msg
    -> ( List (Html msg), List (Html msg), LineBlocks msg )
proccessList x denom blocks =
    case blocks of
        [] ->
            ( [], [], [] )

        h :: t ->
            proccessListItem x denom h t


proccessListItem :
    Int
    -> Denominator
    -> LineBlock msg
    -> LineBlocks msg
    -> ( List (Html msg), List (Html msg), LineBlocks msg )
proccessListItem x denom h t =
    {- Returns:
       ( Blocks to append the list
       , Blocks to append the list item
       , rest of lines)
    -}
    if (h.indent - x) >= 2 && (h.indent - x) < 6 then
        case h.block of
            ListItem denom2 s ->
                let
                    ( currentElement, rest ) =
                        newList h.indent denom2 s t

                    ( html, _, rest2 ) =
                        proccessList x denom rest
                in
                    ( currentElement :: html, [], rest2 )

            other ->
                let
                    ( html, inItem, rest ) =
                        proccessList x denom t
                in
                    ( [], blockToHtml other :: inItem, rest )
    else if (h.indent - x) < 0 then
        ( [], [], h :: t )
    else
        case h.block of
            ListItem denom2 s ->
                if denom == denom2 then
                    let
                        ( html, inItem, rest ) =
                            proccessList x denom t
                    in
                        ( (li [] (text s :: inItem)) :: html
                        , []
                        , rest
                        )
                else
                    ( [], [], t )

            _ ->
                ( [], [], h :: t )



-- Indented code blocks


proccessIndCodeBlock :
    List (LineBlock msg)
    -> ( List String, List (LineBlock msg) )
proccessIndCodeBlock l =
    case l of
        [] ->
            ( [], [] )

        h :: t ->
            if h.indent < 4 then
                ( [], h :: t )
            else
                let
                    src =
                        (String.repeat (h.indent - 4) " ") ++ h.source
                in
                    Tuple.mapFirst ((::) (src)) (proccessIndCodeBlock t)
