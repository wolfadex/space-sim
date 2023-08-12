module FormBuilder exposing (main)

import Control
import Input.Slider.Float
import Percent


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


type Enabled
    = Enabled
    | Disabled
