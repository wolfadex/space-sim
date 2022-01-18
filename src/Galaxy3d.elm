module Galaxy3d exposing (view)

import Angle
import Camera3d
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Element
import Game.Components exposing (GalacticCoordinates)
import Length exposing (Meters)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Pixels
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import Sphere3d
import Viewpoint3d


view : { a | solarSystems : Set EntityID, galaxyPositions : Logic.Component.Set (Point3d Meters GalacticCoordinates) } -> Element.Element msg
view world =
    let
        solarSystems : List (Scene3d.Entity GalacticCoordinates)
        solarSystems =
            List.filterMap (viewSolarSystem world)
                (Set.toList world.solarSystems)
    in
    Element.html
        (Scene3d.unlit
            { entities =
                Scene3d.quad (Material.color Color.black)
                    (Point3d.meters -1.5 -1.5 0)
                    (Point3d.meters 1.5 -1.5 0)
                    (Point3d.meters 1.5 1.5 0)
                    (Point3d.meters -1.5 1.5 0)
                    :: Scene3d.cylinder (Material.color (Color.rgb 0 0.1 0.3))
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius = Length.meters 1.1
                            , length = Length.meters 0.001
                            }
                        )
                    :: solarSystems
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { focalPoint = Point3d.origin
                            , eyePoint = Point3d.meters 5 2 3
                            , upDirection = Direction3d.positiveZ
                            }
                    , verticalFieldOfView = Angle.degrees 30
                    }
            , clipDepth = Length.meters 1
            , background = Scene3d.transparentBackground
            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            }
        )


viewSolarSystem : { a | galaxyPositions : Logic.Component.Set (Point3d Meters GalacticCoordinates) } -> EntityID -> Maybe (Scene3d.Entity GalacticCoordinates)
viewSolarSystem world solarSystemId =
    Maybe.map
        (\position ->
            Scene3d.sphere
                (Material.color Color.gray)
                (Sphere3d.atPoint position (Length.meters 0.025))
        )
        (Logic.Component.get solarSystemId world.galaxyPositions)
