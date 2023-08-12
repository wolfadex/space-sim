module FormBuilder exposing (main)

import Circle2d
import Control exposing (Control)
import CubicSpline2d exposing (CubicSpline2d)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input.MinMaxSlider
import Input.Slider.Float
import Input.Spline
import Json.Decode
import List.Nonempty exposing (Nonempty)
import Numeral
import Percent exposing (Percent)
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
                (\version realisticLighting planetsOrbit planetRotationSpeed showPlanetsOrbit ->
                    { version = version
                    , realisticLighting = realisticLighting
                    , planetsOrbit = planetsOrbit
                    , planetRotationSpeed = planetRotationSpeed
                    , showPlanetsOrbit = showPlanetsOrbit
                    }
                )
                |> Control.hiddenField .version
                    Control.int
                |> Control.field .realisticLighting
                    (Control.bool
                        |> Control.label "Realistic Lighting"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.field .planetsOrbit
                    (Control.bool
                        |> Control.label "Planets Orbit"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.field .planetRotationSpeed
                    (Input.Slider.Float.new { min = 0.0, max = 1.0 }
                        |> Input.Slider.Float.withStep 0.01
                        |> Input.Slider.Float.toControl
                        |> Control.label "Planet Rotation Speed"
                        |> Control.map
                            { convert = Percent.fromFloat
                            , revert = Percent.toFloat
                            }
                    )
                |> Control.field .showPlanetsOrbit
                    (Control.bool
                        |> Control.label "Show Planets Orbit"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.endRecord
        , outputToString = Debug.toString
        }


type alias Settings =
    { version : Int
    , realisticLighting : Enabled
    , planetsOrbit : Enabled
    , planetRotationSpeed : Percent ()
    , showPlanetsOrbit : Enabled
    }


type Enabled
    = Enabled
    | Disabled
