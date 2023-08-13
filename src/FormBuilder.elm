module FormBuilder exposing (main)

import Control
import Data.Name


main =
    Control.sandbox
        { control =
            Control.record
                (\name planetName ->
                    { name = name
                    , planetName = planetName
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
                |> Control.endRecord
        , outputToString = Debug.toString
        }
