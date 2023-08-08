module FormBuilder exposing (main)

import Circle2d
import Control exposing (Control)
import CubicSpline2d exposing (CubicSpline2d)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input.MinMaxSlider
import Input.Slider
import Input.Spline
import Json.Decode
import List.Nonempty exposing (Nonempty)
import Numeral
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Set exposing (Set)
import SubCmd exposing (SubCmd)
import Svg
import Svg.Attributes
import Svg.Events
import Vector2d


main =
    Control.sandbox
        { control =
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
                        |> Control.label "Solar Systems to Generate:"
                        |> Control.initWith { min = 40, max = 80 }
                    )
                |> Control.field
                    (\{ minPlanetsPerSolarSystemToGenerate, maxPlanetsPerSolarSystemToGenerate } ->
                        { min = minPlanetsPerSolarSystemToGenerate, max = maxPlanetsPerSolarSystemToGenerate }
                    )
                    (Input.MinMaxSlider.new { min = 0, max = 40 }
                        |> Input.MinMaxSlider.withStep 1
                        |> Input.MinMaxSlider.toControl
                        |> Control.label "Planets per Solar System:"
                        |> Control.initWith { min = 1, max = 12 }
                    )
                |> Control.field .starCounts starCountControl
                |> Control.endRecord
        , outputToString = Debug.toString
        }


type alias SolarSystem =
    { minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
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



-- minSolarSystemsToGenerate = 40
-- , maxSolarSystemsToGenerate = 80
-- , minPlanetsPerSolarSystemToGenerate = 1
-- , maxPlanetsPerSolarSystemToGenerate = 12
-- , starCounts =
--     List.Nonempty.appendList
--         [ ( 0.33, 2 ), ( 0.08, 3 ), ( 0.01, 4 ), ( 0.01, 5 ), ( 0.01, 6 ), ( 0.01, 7 ) ]
--         (List.Nonempty.singleton ( 0.56, 1 ))
----------------------
-- inputGroup "Solar Systems to Generate"
--     [ Ui.Slider.int []
--         { onChange = GotMinSolarSystemCount
--         , label =
--             Input.labelAbove []
--                 (paragraph []
--                     [ text "Min: "
--                     , displayGameValue "min-solar-system-count" (String.fromInt model.minSolarSystemsToGenerate)
--                     ]
--                 )
--         , min = 10
--         , max = 800
--         , value = model.minSolarSystemsToGenerate
--         , step = Just 10
--         }
--     , Ui.Slider.int []
--         { onChange = GotMaxSolarSystemCount
--         , label =
--             Input.labelAbove []
--                 (paragraph []
--                     [ text "Max : "
--                     , displayGameValue "max-solar-system-count" (String.fromInt model.maxSolarSystemsToGenerate)
--                     ]
--                 )
--         , min = 10
--         , max = 800
--         , value = model.maxSolarSystemsToGenerate
--         , step = Just 10
--         }
--     ]
