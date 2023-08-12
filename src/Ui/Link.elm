module Ui.Link exposing (internal)

import Html exposing (Html)
import Html.Attributes
import Route
import Ui
import Ui.Theme


internal : List (Html.Attribute msg) -> { route : Route.Route, label : Html msg } -> Html msg
internal attributes config =
    Html.a
        ([ -- paddingXY 16 8
           -- ,
           Ui.borderStyle.solid
         , Ui.borderWidth.px3
         , Ui.borderRadius.px3
         , Ui.backgroundColor Ui.Theme.green
         , Html.Attributes.href (Route.toString config.route)
         , Html.Attributes.style "text-decoration" "none"
         , Html.Attributes.style "display" "grid"
         ]
            ++ attributes
        )
        [ config.label
            |> Ui.el
                [ Ui.justifySelf.center
                , Ui.alignSelf.center
                , Ui.width.shrink
                , Ui.padding.xy.rem1.remHalf
                ]
        ]


external : List (Html.Attribute msg) -> { url : String, label : Html msg } -> Html msg
external attributes options =
    Html.a
        (attributes
            ++ [ Html.Attributes.href options.url
               , Html.Attributes.target "_blank"
               ]
        )
        [ options.label
        ]
