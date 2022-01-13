module Testing exposing (viewToDocument)

import Browser exposing (Document)
import Element exposing (..)
import Html exposing (Html)
import View exposing (View)


viewToDocument : (Element msg -> Html msg) -> (model -> View msg) -> model -> Document msg
viewToDocument fn view model =
    let
        renderedView : View msg
        renderedView =
            view model
    in
    { title = renderedView.title
    , body = [ fn renderedView.body ]
    }
