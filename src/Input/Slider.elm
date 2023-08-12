module Input.Slider exposing (Config(..), InternalConfig, InternalModel, Model(..), Msg(..), initEmpty, initWith, new, parse, toControl, update, view, withStep)

import Control exposing (Control)
import Html
import Html.Attributes
import Html.Events


toControl : Config -> Control Model Msg Int
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
    { value : Int
    , config : InternalConfig
    }


type Config
    = Config InternalConfig


new : { min : Int, max : Int } -> Config
new opts =
    Config
        { min = opts.min
        , max = opts.max
        , step = Nothing
        }


withStep : Int -> Config -> Config
withStep step (Config config) =
    Config { config | step = Just step }


type alias InternalConfig =
    { min : Int
    , max : Int
    , step : Maybe Int
    }


initEmpty : InternalConfig -> ( Model, Cmd Msg )
initEmpty config =
    ( Model
        { value = config.min
        , config = config
        }
    , Cmd.none
    )


initWith : InternalConfig -> Int -> ( Model, Cmd Msg )
initWith config value =
    ( Model
        { value = value
        , config = config
        }
    , Cmd.none
    )


type Msg
    = ValueChanged Int


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
            , Html.Attributes.min (String.fromInt model.config.min)
            , Html.Attributes.max (String.fromInt model.config.max)
            , Html.Attributes.value (String.fromInt model.value)
            , case model.config.step of
                Just step ->
                    Html.Attributes.step (String.fromInt step)

                Nothing ->
                    Html.Attributes.class ""
            , Html.Events.onInput
                (String.toInt
                    >> Maybe.withDefault model.value
                    >> ValueChanged
                )
            ]
            []
        ]
    ]
