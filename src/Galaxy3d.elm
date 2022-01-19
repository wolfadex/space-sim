module Galaxy3d exposing (view)

import Angle
import Axis2d
import Axis3d
import Camera3d
import Circle2d
import Color
import Cylinder3d
import Direction2d
import Direction3d
import Element
import Frame2d
import Game.Components exposing (LightYear)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Length exposing (Meters)
import LineSegment2d
import Logic.Component
import Logic.Entity exposing (EntityID)
import Pixels
import Point2d
import Point3d exposing (Point3d)
import Point3d.Projection
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import Sphere3d
import Svg
import Svg.Attributes
import Svg.Events
import Viewpoint3d


view : { a | solarSystems : Set EntityID, galaxyPositions : Logic.Component.Set (Point3d Meters LightYear) } -> (EntityID -> msg) -> Element.Element msg
view world onPress =
    let
        solarSystemPoints : List ( EntityID, Point3d Meters LightYear )
        solarSystemPoints =
            List.filterMap (solarSystemPoint world)
                (Set.toList world.solarSystems)

        solarSystems : List (Scene3d.Entity LightYear)
        solarSystems =
            List.map (Tuple.second >> viewSolarSystem) solarSystemPoints

        width : number
        width =
            800

        height : number
        height =
            600

        -- eyePoint : Point3d Meters coordinates
        -- eyePoint =
        --     Point3d.meters 4 0 0
        --         |> Point3d.rotateAround Axis3d.y (Angle.degrees -22.5)
        --         |> Point3d.rotateAround Axis3d.z (Angle.degrees 60)
        viewpoint : Viewpoint3d.Viewpoint3d Meters coordinates
        viewpoint =
            -- Viewpoint3d.lookAt
            --     { focalPoint = Point3d.origin
            --     , eyePoint = eyePoint
            --     , upDirection = Direction3d.z
            --     }
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.meters 5 2 3
                , upDirection = Direction3d.positiveZ
                }

        camera : Camera3d.Camera3d Meters coordinates
        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }

        -- Take the 3D model for the logo and rotate it by the current angle
        -- rotatedLogo =
        --     blockEntity |> Scene3d.rotateAround Axis3d.z angle
        -- Defines the shape of the 'screen' that we will be using when
        --
        -- projecting 3D points into 2D
        screenRectangle : Rectangle2d.Rectangle2d Pixels.Pixels coordinates
        screenRectangle =
            Rectangle2d.from Point2d.origin (Point2d.pixels width height)

        angle : Angle.Angle
        angle =
            Angle.degrees 0.0

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d : List ( EntityID, Point2d.Point2d Pixels.Pixels coordinates )
        vertices2d =
            List.map
                (Tuple.mapSecond
                    (Point3d.rotateAround Axis3d.z angle
                        >> Point3d.Projection.toScreenSpace camera screenRectangle
                    )
                )
                solarSystemPoints

        svgLabels : List (Svg.Svg msg)
        svgLabels =
            List.map
                (\( solarSystemId, vertex ) ->
                    Svg.g
                        [ Svg.Attributes.class "galactic-label"
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(255, 255, 0)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
                            , Svg.Events.onClick (onPress solarSystemId)

                            -- This isn't working, need to debug for accessibility
                            -- , Html.Attributes.tabindex 0
                            ]
                            (Circle2d.withRadius (Pixels.float 6) vertex)
                        , Geometry.Svg.lineSegment2d
                            [ Svg.Attributes.stroke "red"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDasharray "5 5"
                            , Svg.Attributes.class "galactic-label-ignore"
                            ]
                            (LineSegment2d.from vertex (Point2d.pixels (width / 2) (height - 50)))
                        , -- Hack: flip the text upside down since our later
                          -- 'Svg.relativeTo topLeftFrame' call will flip it
                          -- back right side up
                          Geometry.Svg.mirrorAcross (Axis2d.through (Point2d.fromMeters { x = width / 2, y = height / 2 }) Direction2d.x)
                            (Svg.text_
                                [ Svg.Attributes.fill "red" --"rgb(255, 255, 255)"
                                , Svg.Attributes.fontFamily "monospace"
                                , Svg.Attributes.fontSize "20px"
                                , Svg.Attributes.stroke "none"
                                , Svg.Attributes.x (String.fromFloat (width / 2))
                                , Svg.Attributes.y (String.fromFloat 50)
                                , Svg.Attributes.class "galactic-label-ignore"
                                ]
                                [ Svg.text ("SS_" ++ String.fromInt solarSystemId) ]
                            )
                        ]
                )
                vertices2d

        -- Used for converting from coordinates relative to the bottom-left
        -- corner of the 2D drawing into coordinates relative to the top-left
        -- corner (which is what SVG natively works in)
        topLeftFrame : Frame2d.Frame2d Pixels.Pixels coordinates defines2
        topLeftFrame =
            Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float height))
                |> Frame2d.reverseY

        -- Create an SVG element with the projected points, lines and
        -- associated labels
        solarSystemLabels : Html msg
        solarSystemLabels =
            Svg.svg
                [ Html.Attributes.width width
                , Html.Attributes.height height
                ]
                [ Geometry.Svg.relativeTo topLeftFrame (Svg.g [] svgLabels) ]
    in
    Element.el
        [ Element.inFront (Element.html (Html.node "style" [] [ Html.text """
.galactic-label {
    opacity: 0;
    cursor: pointer;
}

.galactic-label:active,
.galactic-label:focus,
.galactic-label:focus-within,
.galactic-label:hover {
    opacity: 1;
}

.galactic-label-ignore {
    pointer-events: none;
}
""" ]))
        , Element.inFront (Element.html solarSystemLabels)
        ]
        (Element.html
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
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                }
            )
        )


viewSolarSystem : Point3d Meters LightYear -> Scene3d.Entity LightYear
viewSolarSystem position =
    let
        drawPosition : Point3d Meters coordinates
        drawPosition =
            Point3d.fromMeters
                { x = Length.inLightYears (Point3d.xCoordinate position) / 5000
                , y = Length.inLightYears (Point3d.yCoordinate position) / 5000
                , z = Length.inLightYears (Point3d.zCoordinate position) / 5000
                }
    in
    Scene3d.sphere
        (Material.color Color.gray)
        (Sphere3d.atPoint drawPosition (Length.meters 0.025))


solarSystemPoint : { a | galaxyPositions : Logic.Component.Set (Point3d Meters LightYear) } -> EntityID -> Maybe ( EntityID, Point3d Meters LightYear )
solarSystemPoint world solarSystemId =
    Maybe.map (Tuple.pair solarSystemId)
        (Logic.Component.get solarSystemId world.galaxyPositions)
