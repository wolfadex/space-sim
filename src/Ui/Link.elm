module Ui.Link exposing (internal)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Route
import Ui.Theme


internal : { route : Route.Route, label : Element msg } -> Element msg
internal config =
    link
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
        , Border.rounded 3
        , Background.color Ui.Theme.green
        ]
        { url = Route.toString config.route
        , label = config.label
        }
