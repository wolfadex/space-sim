module View exposing (View, viewToTestDocument)

import Browser exposing (Document)
import Element exposing (..)
import Html exposing (Html)


type alias View msg =
    { title : String
    , body : Element msg
    }


viewToTestDocument : (Element msg -> Html msg) -> (model -> View msg) -> model -> Document msg
viewToTestDocument fn view model =
    let
        renderedView : View msg
        renderedView =
            view model
    in
    { title = renderedView.title
    , body = [ fn renderedView.body ]
    }
