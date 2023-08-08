module Input.MinMaxSlider exposing (..)

import Control exposing (Control)
import Html exposing (Html)
import Html.Attributes
import Html.Events


toControl : Config -> Control Model Msg { min : Int, max : Int }
toControl (Config config) =
    Control.create
        { label = "Min-Max Slider"
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
    { minimum : Int
    , maximum : Int
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
        { minimum = config.min
        , maximum = config.max
        , config = config
        }
    , Cmd.none
    )


initWith : InternalConfig -> { min : Int, max : Int } -> ( Model, Cmd Msg )
initWith config value =
    ( Model
        { minimum = value.min
        , maximum = value.max
        , config = config
        }
    , Cmd.none
    )


type Msg
    = MinimumChanged Int
    | MaximumChanged Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        MinimumChanged minimum ->
            ( Model
                { model
                    | minimum = minimum
                    , maximum = max minimum model.maximum
                }
            , Cmd.none
            )

        MaximumChanged maximum ->
            ( Model
                { model
                    | maximum = maximum
                    , minimum = min maximum model.minimum
                }
            , Cmd.none
            )


parse : Model -> Result (List String) { min : Int, max : Int }
parse (Model model) =
    Ok { min = model.minimum, max = model.maximum }


view :
    { state : Model
    , name : String
    , label : String
    , id : String
    , class : String
    }
    -> List (Html Msg)
view { state, name, label, id, class } =
    let
        (Model model) =
            state
    in
    [ Html.span [] [ Html.text label ]
    , Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "gap" "0.25rem"
        , Html.Attributes.style "grid-template-columns" "2rem 1fr"
        , Html.Attributes.style "padding-left" "1rem"
        ]
        (viewSlider
            { id = id ++ "-min"
            , class = class
            , name = name ++ "-min"
            , label = "Min"
            , min = model.config.min
            , max = model.config.max
            , step = model.config.step
            , value = model.minimum
            , onInput = MinimumChanged
            }
            ++ viewSlider
                { id = id ++ "-max"
                , class = class
                , name = name ++ "-max"
                , label = "Max"
                , min = model.config.min
                , max = model.config.max
                , step = model.config.step
                , value = model.maximum
                , onInput = MaximumChanged
                }
        )
    ]


viewSlider :
    { id : String
    , class : String
    , name : String
    , label : String
    , min : Int
    , max : Int
    , step : Maybe Int
    , value : Int
    , onInput : Int -> Msg
    }
    -> List (Html Msg)
viewSlider options =
    [ Html.label
        [ Html.Attributes.for options.name
        , Html.Attributes.style "gap" "1rem"
        ]
        [ Html.text options.label ]
    , Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.name options.name
        , Html.Attributes.min (String.fromInt options.min)
        , Html.Attributes.max (String.fromInt options.max)
        , Html.Attributes.value (String.fromInt options.value)
        , case options.step of
            Just step ->
                Html.Attributes.step (String.fromInt step)

            Nothing ->
                Html.Attributes.class ""
        , Html.Events.onInput
            (String.toInt
                >> Maybe.withDefault options.value
                >> options.onInput
            )
        ]
        []
    ]
