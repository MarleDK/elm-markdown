module Efficiency exposing (..)

import Html exposing (..)
import Markdown
import Time exposing (Time)
import Task
import LongMarkdown


main : Program Never (Model msg) Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model msg =
    { time : Time
    , times : List Time
    , md : Html msg
    , testLengths : List Int
    }


type Msg
    = NewTime Time
    | StartTime Time


listOfTests : List Int
listOfTests =
    List.map ((*) 5) (List.range 1 100)


longMarkdown : Int -> String
longMarkdown i =
    String.join "\n" <|
        List.repeat i LongMarkdown.longMarkdown


update : Msg -> Model msg -> ( Model msg, Cmd Msg )
update msg model =
    case msg of
        NewTime time ->
            case model.testLengths of
                [] ->
                    ( { model
                        | times = (time - model.time) :: model.times
                      }
                    , Cmd.none
                    )

                h :: t ->
                    ( { model
                        | times = (time - model.time) :: model.times
                        , time = time
                        , md = Markdown.toHtml (longMarkdown h)
                        , testLengths = t
                      }
                    , Task.perform NewTime Time.now
                    )

        StartTime time ->
            case model.testLengths of
                [] ->
                    ( model
                    , Cmd.none
                    )

                h :: t ->
                    ( { model
                        | time = time
                        , md = Markdown.toHtml (longMarkdown h)
                        , testLengths = t
                      }
                    , Task.perform NewTime Time.now
                    )


view : Model msg -> Html Msg
view model =
    div []
        [ p [] [ text "Efficiency test of parsing Markdown" ]
        , p [] [ text ("Time taken: " ++ (toString (model.times))) ]
        ]


subscriptions : Model msg -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model msg, Cmd Msg )
init =
    ( Model 0 [] (text "") listOfTests, Task.perform StartTime Time.now )
