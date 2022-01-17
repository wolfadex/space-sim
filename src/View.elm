module View exposing (View, map, viewToTestDocument)

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


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn view =
    { title = view.title
    , body = Element.map fn view.body
    }
