module Galaxy3d exposing
    ( viewGalaxy
    , viewSolarSystem
    )

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
import Game.Components exposing (AstronomicalUnit, LightYear, StarSize(..), World)
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


viewGalaxy : World -> (EntityID -> msg) -> Element.Element msg
viewGalaxy world onPress =
    let
        solarSystemPoints : List ( EntityID, Point3d Meters LightYear )
        solarSystemPoints =
            List.filterMap (solarSystemPoint world)
                (Set.toList world.solarSystems)

        solarSystems : List (Scene3d.Entity ScaledViewPoint)
        solarSystems =
            List.map (Tuple.second >> renderSolarSystem) solarSystemPoints

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
        vertices2d : List ( EntityID, Point2d.Point2d Pixels.Pixels ScaledViewPoint )
        vertices2d =
            List.map
                (Tuple.mapSecond
                    (scalePointInLightYearsToOne
                        >> Point3d.rotateAround Axis3d.z angle
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
        galaxyLabels : Html msg
        galaxyLabels =
            Svg.svg
                [ Html.Attributes.width width
                , Html.Attributes.height height
                ]
                [ Geometry.Svg.relativeTo topLeftFrame (Svg.g [] svgLabels) ]

        galaxyScene : Html msg
        galaxyScene =
            Scene3d.unlit
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
    in
    viewSpace galaxyLabels galaxyScene


viewSolarSystem :
    { onPressStar : EntityID -> msg
    , onPressPlanet : EntityID -> msg
    , stars : Set EntityID
    , planets : Set EntityID
    }
    -> World
    -> Element.Element msg
viewSolarSystem { onPressStar, onPressPlanet, stars, planets } world =
    let
        planetPoints : List ( EntityID, Point3d Meters AstronomicalUnit )
        planetPoints =
            List.filterMap (planetPoint world)
                (Set.toList planets)

        starPoints : List StarRenderDetails
        starPoints =
            List.filterMap (starPoint world)
                (Set.toList stars)

        planetEntities : List (Scene3d.Entity ScaledViewPoint)
        planetEntities =
            List.map (Tuple.second >> renderPlanet) planetPoints

        starEntities : List (Scene3d.Entity ScaledViewPoint)
        starEntities =
            List.map renderStar starPoints

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

        planetVertices2d : List ( EntityID, Point2d.Point2d Pixels.Pixels ScaledViewPoint )
        planetVertices2d =
            List.map
                (Tuple.mapSecond
                    (scalePointInAstroUnitsToOne
                        >> Point3d.rotateAround Axis3d.z angle
                        >> Point3d.Projection.toScreenSpace camera screenRectangle
                    )
                )
                planetPoints

        planetLabels : List (Svg.Svg msg)
        planetLabels =
            List.map
                (\( planetId, vertex ) ->
                    Svg.g
                        [ Svg.Attributes.class "galactic-label"
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(255, 255, 0)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
                            , Svg.Events.onClick (onPressPlanet planetId)

                            -- This isn't working, need to debug for accessibility
                            -- , Html.Attributes.tabindex 0
                            ]
                            (Circle2d.withRadius (Pixels.float 10) vertex)
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
                                [ Svg.text ("P_" ++ String.fromInt planetId) ]
                            )
                        ]
                )
                planetVertices2d

        starVertices2d : List ( EntityID, Float, Point2d.Point2d Pixels.Pixels ScaledViewPoint )
        starVertices2d =
            List.map
                (\details ->
                    ( details.id
                    , details.size
                    , scalePointInAstroUnitsToOne details.position
                        |> Point3d.rotateAround Axis3d.z angle
                        |> Point3d.Projection.toScreenSpace camera screenRectangle
                    )
                )
                starPoints

        starLabels : List (Svg.Svg msg)
        starLabels =
            List.map
                (\( starId, size, vertex ) ->
                    Svg.g
                        [ Svg.Attributes.class "galactic-label"
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(255, 255, 0)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
                            , Svg.Events.onClick (onPressStar starId)

                            -- This isn't working, need to debug for accessibility
                            -- , Html.Attributes.tabindex 0
                            ]
                            (Circle2d.withRadius (Pixels.float (190 * size)) vertex)
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
                                [ Svg.text ("S_" ++ String.fromInt starId) ]
                            )
                        ]
                )
                starVertices2d

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
                [ Geometry.Svg.relativeTo topLeftFrame (Svg.g [] (starLabels ++ planetLabels)) ]

        solarSystemScene : Html msg
        solarSystemScene =
            Scene3d.unlit
                { entities =
                    Scene3d.quad (Material.color Color.black)
                        (Point3d.meters -1.5 -1.5 0)
                        (Point3d.meters 1.5 -1.5 0)
                        (Point3d.meters 1.5 1.5 0)
                        (Point3d.meters -1.5 1.5 0)
                        :: planetEntities
                        ++ starEntities
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                }
    in
    viewSpace solarSystemLabels solarSystemScene


viewSpace : Html msg -> Html msg -> Element.Element msg
viewSpace labels scene =
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
        , Element.inFront (Element.html labels)
        ]
        (Element.html scene)



-- Helpers


renderSolarSystem : Point3d Meters LightYear -> Scene3d.Entity ScaledViewPoint
renderSolarSystem position =
    Scene3d.sphere
        (Material.color Color.gray)
        (Sphere3d.atPoint (scalePointInLightYearsToOne position) (Length.meters 0.025))


solarSystemPoint : World -> EntityID -> Maybe ( EntityID, Point3d Meters LightYear )
solarSystemPoint world solarSystemId =
    Maybe.map (Tuple.pair solarSystemId)
        (Logic.Component.get solarSystemId world.galaxyPositions)


type ScaledViewPoint
    = ScaledViewPoint Never


scalePointInLightYearsToOne : Point3d Meters LightYear -> Point3d Meters ScaledViewPoint
scalePointInLightYearsToOne point =
    Point3d.fromMeters
        { x = Length.inLightYears (Point3d.xCoordinate point) / 5000
        , y = Length.inLightYears (Point3d.yCoordinate point) / 5000
        , z = Length.inLightYears (Point3d.zCoordinate point) / 5000
        }


renderPlanet : Point3d Meters AstronomicalUnit -> Scene3d.Entity ScaledViewPoint
renderPlanet position =
    Scene3d.sphere
        (Material.color Color.blue)
        (Sphere3d.atPoint (scalePointInAstroUnitsToOne position) (Length.meters 0.05))


scalePointInAstroUnitsToOne : Point3d Meters AstronomicalUnit -> Point3d Meters ScaledViewPoint
scalePointInAstroUnitsToOne point =
    Point3d.fromMeters
        { x = Length.inAstronomicalUnits (Point3d.xCoordinate point) / 12
        , y = Length.inAstronomicalUnits (Point3d.yCoordinate point) / 12
        , z = Length.inAstronomicalUnits (Point3d.zCoordinate point) / 12
        }


planetPoint : World -> EntityID -> Maybe ( EntityID, Point3d Meters AstronomicalUnit )
planetPoint world planetId =
    Maybe.map
        (\orbit ->
            ( planetId
            , Point3d.fromMeters
                { x = Length.inMeters (Length.astronomicalUnits (toFloat orbit))
                , y = 0
                , z = 0
                }
            )
        )
        (Logic.Component.get planetId world.orbits)


renderStar : StarRenderDetails -> Scene3d.Entity ScaledViewPoint
renderStar details =
    Scene3d.sphere
        (Material.color details.color)
        (Sphere3d.atPoint (scalePointInAstroUnitsToOne details.position) (Length.meters details.size))


type alias StarRenderDetails =
    { id : EntityID
    , color : Color.Color
    , position : Point3d Meters AstronomicalUnit
    , size : Float
    }


starPoint : World -> EntityID -> Maybe StarRenderDetails
starPoint world starId =
    Maybe.map
        (\size ->
            { id = starId
            , color =
                case size of
                    Yellow ->
                        Color.yellow

                    RedGiant ->
                        Color.red

                    BlueGiant ->
                        Color.blue

                    WhiteDwarf ->
                        Color.white

                    BlackDwarf ->
                        Color.black
            , position =
                Point3d.fromMeters
                    { x = 0
                    , y = 0
                    , z = 0
                    }
            , size =
                case size of
                    Yellow ->
                        0.1

                    RedGiant ->
                        0.2

                    BlueGiant ->
                        0.3

                    WhiteDwarf ->
                        0.05

                    BlackDwarf ->
                        0.03
            }
        )
        (Logic.Component.get starId world.starForms)
