module BlockToHtml exposing (processBlocks)

import BlockType exposing (Denominator(..), LineBlock, LineBlocks, Block(..), Blocks, hlToHTML, blockToHtml)
import Html exposing (..)
import Tuple


{-
   This module is responsible for transforming a list og Blocks to the right
   HTML representation.
   This requires some context when processing the list.
-}


processBlocks : LineBlocks -> Html msg
processBlocks blocks =
    div [] (processBlocksHelp blocks)


processBlocksHelp : LineBlocks -> List (Html msg)
processBlocksHelp blocks =
    case blocks of
        [] ->
            []

        h :: t ->
            processNonEmptyBlocksHelp h t


processNonEmptyBlocksHelp : LineBlock -> LineBlocks -> List (Html msg)
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
                -- TODO --
                (p [] [ text s ])
                    :: (p [] [ text "Paragraph rendering not finished" ])
                    :: (processBlocksHelp t)
    else
        -- TODO: multiline --
        [ (pre [] [ code [] [ text h.source ] ]) ]


newList : Int -> Denominator -> String -> LineBlocks -> ( Html msg, LineBlocks )
newList x denom s t =
    let
        ( html, inItem, restOfBlocks ) =
            proccessList x denom t

        listItems =
            (li [] (text s :: inItem)) :: html
    in
        ( (ul [] listItems), restOfBlocks )


proccessList : Int -> Denominator -> LineBlocks -> ( List (Html msg), List (Html msg), LineBlocks )
proccessList x denom blocks =
    case blocks of
        [] ->
            ( [], [], [] )

        h :: t ->
            proccessListItem x denom h t


proccessListItem : Int -> Denominator -> LineBlock -> LineBlocks -> ( List (Html msg), List (Html msg), LineBlocks )
proccessListItem x denom h t =
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
                -- TODO --
                let
                    ( html, inItem, rest ) =
                        proccessList x denom t
                in
                    ( [], blockToHtml other :: inItem, rest )
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
                -- TODO --
                ( [], [], h :: t )
