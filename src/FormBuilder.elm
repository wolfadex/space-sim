module FormBuilder exposing (main)

import Control
import Data.Name
import Input.Spline


main =
    Control.sandbox
        { control =
            Control.record
                (\name planetName reproductionMotivation ->
                    { name = name
                    , planetName = planetName
                    , reproductionMotivation = reproductionMotivation
                    }
                )
                |> Control.field .name
                    (Control.string
                        |> Control.label "Name"
                        |> Control.failIf String.isEmpty "Name cannot be empty"
                        |> Control.map
                            { convert = Data.Name.fromString
                            , revert = Data.Name.toString
                            }
                    )
                |> Control.field .planetName
                    (Control.string
                        |> Control.label "Home Planet Name"
                        |> Control.failIf String.isEmpty "Home Planet Name cannot be empty"
                    )
                |> Control.field .reproductionMotivation
                    (Input.Spline.new
                        { xMin = 0
                        , xMax = 1
                        , yMin = 0
                        , yMax = 1
                        }
                        |> Input.Spline.toControl
                        |> Control.label "Reproduction Motivation"
                    )
                |> Control.endRecord
        , outputToString = Debug.toString
        }
