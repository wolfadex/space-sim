module Page.Shared exposing (GalaxyFormDelta, GalaxyFormResult, GalaxyFormState, galaxyForm)

import Control exposing (Control)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input.MinMaxSlider
import List.Nonempty exposing (Nonempty)
import Numeral


type alias GalaxyFormState =
    ( Control.State Input.MinMaxSlider.Model
    , ( Control.State Input.MinMaxSlider.Model
      , ( Control.State (Nonempty ( Float, Int ))
        , Control.End
        )
      )
    )


type alias GalaxyFormDelta =
    ( Control.Delta Input.MinMaxSlider.Msg
    , ( Control.Delta Input.MinMaxSlider.Msg
      , ( Control.Delta ( Int, Float ), Control.End )
      )
    )


type alias GalaxyFormResult =
    { minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
    }


galaxyForm : { toMsg : Control.Delta GalaxyFormDelta -> msg, onSubmit : msg } -> Control.Form GalaxyFormState GalaxyFormDelta GalaxyFormResult msg
galaxyForm config =
    Control.form
        { onUpdate = config.toMsg
        , view =
            \fields ->
                Html.form
                    [ Html.Events.onSubmit config.onSubmit ]
                    fields
        , control =
            Control.record
                (\solarSystems planets starCounts ->
                    { minSolarSystemsToGenerate = solarSystems.min
                    , maxSolarSystemsToGenerate = solarSystems.max
                    , minPlanetsPerSolarSystemToGenerate = planets.min
                    , maxPlanetsPerSolarSystemToGenerate = planets.max
                    , starCounts = starCounts
                    }
                )
                |> Control.field
                    (\{ minSolarSystemsToGenerate, maxSolarSystemsToGenerate } ->
                        { min = minSolarSystemsToGenerate, max = maxSolarSystemsToGenerate }
                    )
                    (Input.MinMaxSlider.new { min = 10, max = 800 }
                        |> Input.MinMaxSlider.withStep 10
                        |> Input.MinMaxSlider.toControl
                        |> Control.label "Solar Systems to Generate"
                        |> Control.initWith { min = 40, max = 80 }
                        |> Control.noteIf (\val -> val.min > 600) "This may be slow to generate."
                    )
                |> Control.field
                    (\{ minPlanetsPerSolarSystemToGenerate, maxPlanetsPerSolarSystemToGenerate } ->
                        { min = minPlanetsPerSolarSystemToGenerate, max = maxPlanetsPerSolarSystemToGenerate }
                    )
                    (Input.MinMaxSlider.new { min = 0, max = 40 }
                        |> Input.MinMaxSlider.withStep 1
                        |> Input.MinMaxSlider.toControl
                        |> Control.label "Planets per Solar System"
                        |> Control.initWith { min = 1, max = 12 }
                        |> Control.noteIf (\val -> val.max == 0) "This will be a very empty galaxy!"
                    )
                |> Control.field .starCounts starCountControl
                |> Control.endRecord
        }


starCountControl : Control (Nonempty ( Float, Int )) ( Int, Float ) (Nonempty ( Float, Int ))
starCountControl =
    Control.create
        { label = "Odds that a Solar System has:"
        , initEmpty =
            ( List.Nonempty.appendList
                [ ( 0.33, 2 ), ( 0.08, 3 ), ( 0.01, 4 ), ( 0.01, 5 ), ( 0.01, 6 ), ( 0.01, 7 ) ]
                (List.Nonempty.singleton ( 0.56, 1 ))
            , Cmd.none
            )
        , initWith = \starCounts -> ( starCounts, Cmd.none )
        , update = updateStarCountControl
        , view = viewStarCountControl
        , subscriptions = \_ -> Sub.none
        , parse = Ok
        }


updateStarCountControl : ( Int, Float ) -> Nonempty ( Float, Int ) -> ( Nonempty ( Float, Int ), Cmd ( Int, Float ) )
updateStarCountControl ( index, newPercent ) starCounts =
    let
        originalPercent : Float
        originalPercent =
            List.Nonempty.toList starCounts
                |> List.drop index
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault newPercent

        otherStarChange : Float
        otherStarChange =
            (originalPercent - newPercent) / toFloat (List.Nonempty.length starCounts - 1)
    in
    ( List.Nonempty.indexedMap
        (\i ( percent, count ) ->
            ( if i == index then
                newPercent

              else
                min 1 (max 0 (percent + otherStarChange))
            , count
            )
        )
        starCounts
    , Cmd.none
    )


viewStarCountControl :
    { state : Nonempty ( Float, Int )
    , class : String
    , id : String
    , name : String
    , label : String
    }
    -> List (Html ( Int, Float ))
viewStarCountControl { state, class, id, name, label } =
    [ Html.label
        [ Html.Attributes.for name ]
        [ Html.text label ]
    , Html.div
        [ Html.Attributes.class class
        , Html.Attributes.id id
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "padding-left" "1rem"
        ]
        (state
            |> List.Nonempty.toList
            |> List.concatMap
                (\( percent, count ) ->
                    [ Html.label
                        [ Html.Attributes.for (name ++ "stars-" ++ String.fromInt count) ]
                        [ Html.text (String.fromInt count ++ " Stars: " ++ Numeral.format "0.00[%]" percent) ]
                    , Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Attributes.name name
                        , Html.Attributes.min (String.fromFloat 0.0)
                        , Html.Attributes.max (String.fromFloat 1.0)
                        , Html.Attributes.step (String.fromFloat 0.001)
                        , Html.Attributes.value (String.fromFloat percent)
                        , Html.Events.onInput
                            (String.toFloat
                                >> Maybe.withDefault percent
                                >> Tuple.pair (count - 1)
                            )
                        ]
                        []
                    ]
                )
        )
    ]
