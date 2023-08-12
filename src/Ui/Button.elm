module Ui.Button exposing
    ( default
    , inspect
    , negative
    , primary
    , toggle
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Ui
import Ui.Theme


toggle : { onPress : Maybe msg, label : Html msg, enabled : Bool } -> Html msg
toggle config =
    Html.button
        [ Ui.padding.xy.rem1.remHalf
        , Ui.borderStyle.solid
        , Ui.borderWidth.px3
        , Ui.borderRadius.px3
        , Ui.backgroundColor
            (if config.enabled then
                Ui.Theme.darkGray

             else
                Ui.Theme.green
            )
        , Ui.fontColor
            (if config.enabled then
                Ui.Theme.nearlyWhite

             else
                Ui.Theme.darkGray
            )
        , Ui.borderColor Ui.Theme.darkGray
        , case config.onPress of
            Nothing ->
                Ui.noAttribute

            Just onPress ->
                Html.Events.onClick onPress
        ]
        [ config.label ]


primary : { onPress : Maybe msg, label : Html msg } -> Html msg
primary config =
    Html.button
        [ Ui.padding.xy.rem1.remHalf
        , Ui.borderStyle.solid
        , Ui.borderWidth.px3
        , Ui.borderRadius.px3
        , Ui.backgroundColor Ui.Theme.green
        , Ui.fontColor Ui.Theme.darkGray
        , Ui.borderColor Ui.Theme.darkGray
        , case config.onPress of
            Nothing ->
                Ui.noAttribute

            Just onPress ->
                Html.Events.onClick onPress
        ]
        [ config.label ]


negative : { onPress : Maybe msg, label : Html msg } -> Html msg
negative config =
    Html.button
        [ Ui.padding.xy.rem1.remHalf
        , Ui.borderStyle.solid
        , Ui.borderWidth.px3
        , Ui.borderRadius.px3
        , Ui.backgroundColor Ui.Theme.error
        , Ui.fontColor Ui.Theme.nearlyWhite
        , Ui.borderColor Ui.Theme.darkGray
        , case config.onPress of
            Nothing ->
                Ui.noAttribute

            Just onPress ->
                Html.Events.onClick onPress
        ]
        [ config.label ]


default : List (Html.Attribute msg) -> { onPress : Maybe msg, label : Html msg } -> Html msg
default attributes config =
    Html.button
        ([ Ui.padding.xy.rem1.remHalf
         , Ui.borderStyle.solid
         , Ui.borderWidth.px3
         , Ui.borderRadius.px3
         , Ui.backgroundColor Ui.Theme.green
         , case config.onPress of
            Nothing ->
                Ui.noAttribute

            Just onPress ->
                Html.Events.onClick onPress
         ]
            ++ attributes
        )
        [ config.label ]


inspect : Maybe msg -> Html msg
inspect onPress =
    Html.button
        [ Ui.padding.xy.rem1.remHalf
        , Ui.borderStyle.solid
        , Ui.borderWidth.px3
        , Ui.borderRadius.px3
        , Ui.backgroundColor Ui.Theme.darkGray
        , Ui.fontColor Ui.Theme.nearlyWhite
        , Ui.borderColor Ui.Theme.darkGray
        , case onPress of
            Nothing ->
                Ui.noAttribute

            Just onP ->
                Html.Events.onClick onP
        ]
        [ Ui.text "👁" ]
