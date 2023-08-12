module Input.Slider.Float exposing (..)

import Control exposing (Control)
import Html exposing (Html)
import Html.Attributes
import Html.Events


toControl : Config -> Control Model Msg Float
toControl (Config config) =
    Control.create
        { label = "Slider"
        , initEmpty = initEmpty config
        , initWith = initWith config
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , parse = parse
        }


type Model
    = Model InternalModel


type alias InternalModel =
    { value : Float
    , config : InternalConfig
    }


type Config
    = Config InternalConfig


new : { min : Float, max : Float } -> Config
new opts =
    Config
        { min = opts.min
        , max = opts.max
        , step = Nothing
        }


withStep : Float -> Config -> Config
withStep step (Config config) =
    Config { config | step = Just step }


type alias InternalConfig =
    { min : Float
    , max : Float
    , step : Maybe Float
    }


initEmpty : InternalConfig -> ( Model, Cmd Msg )
initEmpty config =
    ( Model
        { value = config.min
        , config = config
        }
    , Cmd.none
    )


initWith : InternalConfig -> Float -> ( Model, Cmd Msg )
initWith config value =
    ( Model
        { value = value
        , config = config
        }
    , Cmd.none
    )


type Msg
    = ValueChanged Float


update : Msg -> Model -> ( Model, Cmd Msg )
update (ValueChanged value) (Model model) =
    ( Model
        { model
            | value = value
        }
    , Cmd.none
    )


parse (Model model) =
    Ok model.value


view { state, name, label, id, class } =
    let
        (Model model) =
            state
    in
    [ Html.div
        [ Html.Attributes.id id
        , Html.Attributes.class class
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "1rem"
        ]
        [ Html.label
            [ Html.Attributes.for name ]
            [ Html.text label ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.name name
            , Html.Attributes.min (String.fromFloat model.config.min)
            , Html.Attributes.max (String.fromFloat model.config.max)
            , Html.Attributes.value (String.fromFloat model.value)
            , case model.config.step of
                Just step ->
                    Html.Attributes.step (String.fromFloat step)

                Nothing ->
                    Html.Attributes.class ""
            , Html.Events.onInput
                (String.toFloat
                    >> Maybe.withDefault model.value
                    >> ValueChanged
                )
            ]
            []
        ]
    ]
