module Name exposing (Model, Msg, update, view, subscriptions, init)

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
    { time1 : Time
    , time2 : Time
    , md : Html msg
    }


type Msg
    = StartTime Time
    | EndTime Time


update : Msg -> Model msg -> ( Model msg, Cmd Msg )
update msg model =
    case msg of
        StartTime time ->
            ( { model
                | time1 = time
                , md = Markdown.toHtml LongMarkdown.longMarkdown
              }
            , Task.perform EndTime Time.now
            )

        EndTime time ->
            ( { model | time2 = time }, Cmd.none )


view : Model msg -> Html Msg
view model =
    div []
        [ p [] [ text "New Html Program" ]
        , p [] [ text ("Start time: " ++ (toString model.time1)) ]
        , p [] [ text ("End time: " ++ (toString model.time2)) ]
        , p [] [ text ("Time taken: " ++ (toString (model.time2 - model.time1))) ]
        ]


subscriptions : Model msg -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model msg, Cmd Msg )
init =
    ( Model 0 0 (text ""), Task.perform StartTime Time.now )
