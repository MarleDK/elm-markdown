import Html exposing (Html, text, div, textarea)
import Html.Events exposing (onInput)
import Markdown2
import Markdown



main =
    Html.beginnerProgram
        { model = ""
        , update = update
        , view = view
        }


type Msg =
    Update String


update : Msg -> String -> String
update (Update s) _ =
    s


view : String -> Html Msg
view model =
    div []
        [ textarea [ onInput Update ] []
        , Markdown.toHtml model
        ]
