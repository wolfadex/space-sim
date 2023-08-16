module Galaxy3d exposing
    ( MinRenderableWorld
    , getGalaxyViewport
    , viewGalaxy
    , viewSolarSystem
    )

import Angle
import Arc3d
import Axis2d
import Axis3d
import Browser.Dom exposing (Viewport)
import Camera3d
import Circle2d
import Circle3d
import Color
import Cylinder3d
import Data.EarthYear
import Data.Orbit exposing (Orbit)
import Data.Star
import Dict exposing (Dict)
import Direction2d
import Direction3d
import Frame2d
import Game.Components exposing (CelestialBodyForm(..), LightYear, SolarSystem, Water)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Value)
import Length exposing (Length, Meters)
import LineSegment2d
import LineSegment3d.Projection
import Logic.Component
import Logic.Entity exposing (EntityID)
import Luminance
import LuminousFlux
import Percent exposing (Percent)
import Pixels
import Point2d
import Point3d exposing (Point3d)
import Point3d.Projection
import Polyline2d
import Polyline3d
import Population exposing (Population)
import Process
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material
import Scene3d.Mesh
import Set exposing (Set)
import Shared exposing (Enabled(..), Settings)
import Sphere3d
import SubCmd exposing (SubCmd)
import Svg
import Svg.Attributes
import Svg.Events
import Task
import Temperature exposing (Temperature)
import Ui
import Ui.Button
import Viewpoint3d


type alias MinRenderableWorld r =
    { r
        | elapsedTime : Float
        , galaxyViewSize : { width : Float, height : Float }
        , zoom : Float
        , viewRotation : Float
        , civilizationPopulations : Logic.Component.Set (Dict EntityID Population)
        , planetTypes : Logic.Component.Set CelestialBodyForm
        , starTemperature : Logic.Component.Set Temperature
        , orbits : Logic.Component.Set Orbit
        , waterContent : Logic.Component.Set (Percent Water)
        , planetSize : Logic.Component.Set Float
        , parents : Logic.Component.Set EntityID
        , galaxyPositions : Logic.Component.Set (Point3d Meters LightYear)
        , solarSystems : Logic.Component.Set SolarSystem
        , planets : Set EntityID
        , stars : Set EntityID
        , civilizations : Set EntityID
    }


type AstronomicalUnit
    = AstronomicalUnit Never


viewGalaxy :
    { onPressSolarSystem : EntityID -> msg
    , onZoom : Value -> msg
    , onZoomPress : Float -> msg
    , onRotationPress : Float -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> MinRenderableWorld r
    -> Html msg
viewGalaxy { onPressSolarSystem, onZoom, onZoomPress, onRotationPress, focusedCivilization } world =
    let
        solarSystemPoints : List ( EntityID, Point3d Meters LightYear )
        solarSystemPoints =
            Dict.merge
                (\_ _ res -> res)
                (\id _ point res -> ( id, point ) :: res)
                (\_ _ res -> res)
                (Logic.Component.toDict world.solarSystems)
                (Logic.Component.toDict world.galaxyPositions)
                []

        solarSystems : List (Scene3d.Entity ScaledViewPoint)
        solarSystems =
            List.map (\( _, point ) -> renderSolarSystem point) solarSystemPoints

        eyePoint : Point3d Meters coordinates
        eyePoint =
            Point3d.rotateAround Axis3d.z
                (Angle.degrees world.viewRotation)
                (Point3d.scaleAbout Point3d.origin
                    world.zoom
                    (Point3d.meters 5 2 3)
                )

        -- |> Point3d.rotateAround Axis3d.y (Angle.degrees -22.5)
        viewpoint : Viewpoint3d.Viewpoint3d Meters coordinates
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
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
            Point2d.pixels world.galaxyViewSize.width world.galaxyViewSize.height
                |> Rectangle2d.from Point2d.origin

        angle : Angle.Angle
        angle =
            Angle.degrees 0.0

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d : List ( EntityID, Point2d.Point2d Pixels.Pixels ScaledViewPoint )
        vertices2d =
            List.map
                (Tuple.mapSecond
                    (\point ->
                        Point3d.Projection.toScreenSpace camera
                            screenRectangle
                            (Point3d.rotateAround Axis3d.z angle (scalePointInLightYearsToOne point))
                    )
                )
                solarSystemPoints

        svgLabels : List (Svg.Svg msg)
        svgLabels =
            List.map
                (\( solarSystemId, vertex ) ->
                    let
                        highlightSolarSystem : Bool
                        highlightSolarSystem =
                            List.any
                                (\civId ->
                                    world.civilizationPopulations
                                        |> Logic.Component.get civId
                                        |> Maybe.map
                                            (\dictPlanetPopulatiopns ->
                                                let
                                                    solarSystemsCivIsIn : List EntityID
                                                    solarSystemsCivIsIn =
                                                        List.filterMap
                                                            (\planetId ->
                                                                Logic.Component.get planetId world.parents
                                                            )
                                                            (Dict.keys dictPlanetPopulatiopns)
                                                in
                                                List.any ((==) solarSystemId) solarSystemsCivIsIn && Just civId == focusedCivilization
                                            )
                                        |> Maybe.withDefault False
                                )
                                (Set.toList world.civilizations)
                    in
                    Svg.g
                        [ Svg.Attributes.class
                            (if highlightSolarSystem then
                                "galactic-label-focus-civ"

                             else
                                "galactic-label"
                            )
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke
                                (if highlightSolarSystem then
                                    "rgb(200, 255, 200)"

                                 else
                                    "rgb(255, 255, 0)"
                                )
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
                            , Svg.Events.onClick (onPressSolarSystem solarSystemId)

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
                            (LineSegment2d.from vertex (Point2d.pixels (world.galaxyViewSize.width / 2) (world.galaxyViewSize.height - 50)))
                        , -- Hack: flip the text upside down since our later
                          -- 'Svg.relativeTo topLeftFrame' call will flip it
                          -- back right side up
                          Geometry.Svg.mirrorAcross (Axis2d.through (Point2d.fromMeters { x = world.galaxyViewSize.width / 2, y = world.galaxyViewSize.height / 2 }) Direction2d.x)
                            (Svg.text_
                                [ Svg.Attributes.fill "red" --"rgb(255, 255, 255)"
                                , Svg.Attributes.fontFamily "monospace"
                                , Svg.Attributes.fontSize "20px"
                                , Svg.Attributes.stroke "none"
                                , Svg.Attributes.x (String.fromFloat (world.galaxyViewSize.width / 2))
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
            Frame2d.reverseY (Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float world.galaxyViewSize.height)))

        -- Create an SVG element with the projected points, lines and
        -- associated labels
        galaxyLabels : Html msg
        galaxyLabels =
            Svg.svg
                [ Html.Attributes.width (floor world.galaxyViewSize.width)
                , Html.Attributes.height (floor world.galaxyViewSize.height)
                ]
                [ Geometry.Svg.relativeTo topLeftFrame (Svg.g [] svgLabels) ]

        galaxyScene : Html msg
        galaxyScene =
            Scene3d.unlit
                { entities =
                    Scene3d.cylinder (Material.color (Color.rgb 0 0.1 0.3))
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius =
                                -- The radius of the Milky Way plus a little
                                Length.lightYears 52000
                            , length = Length.meters 0.01
                            }
                        )
                        :: solarSystems
                , camera = camera
                , clipDepth = Length.nanometer
                , background = Scene3d.backgroundColor Color.black
                , dimensions =
                    ( Pixels.pixels (floor world.galaxyViewSize.width)
                    , Pixels.pixels (floor world.galaxyViewSize.height)
                    )
                }
    in
    viewSpace { onZoom = Just onZoom, onZoomPress = Just onZoomPress, onRotationPress = Just onRotationPress } galaxyLabels galaxyScene


viewSolarSystem :
    { onPressStar : Maybe (EntityID -> msg)
    , onPressPlanet : Maybe (EntityID -> msg)
    , onZoom : Maybe (Value -> msg)
    , onZoomPress : Maybe (Float -> msg)
    , onRotationPress : Maybe (Float -> msg)
    , focusedCivilization : Maybe EntityID
    , stars : Set EntityID
    , planets : Set EntityID
    }
    -> Settings
    -> MinRenderableWorld r
    -> Html msg
viewSolarSystem options settings world =
    let
        planetDetails : List PlanetRenderDetails
        planetDetails =
            List.reverse
                (List.sortBy (\p -> Length.inMeters p.orbitDistance)
                    (List.filterMap (getPlanetDetails settings world)
                        (Set.toList options.planets)
                    )
                )

        starDetails : List StarRenderDetails
        starDetails =
            List.filterMap (getStarDetails world)
                (Set.toList options.stars)

        planetEntities : List (Scene3d.Entity ScaledViewPoint)
        planetEntities =
            List.concatMap (renderPlanet settings) planetDetails

        starEntities : List (Scene3d.Entity ScaledViewPoint)
        starEntities =
            List.map (renderStar settings) starDetails

        eyePoint : Point3d Meters coordinates
        eyePoint =
            Point3d.rotateAround Axis3d.z
                (Angle.degrees world.viewRotation)
                (Point3d.scaleAbout Point3d.origin
                    world.zoom
                    (Point3d.meters 5 2 3)
                )

        -- |> Point3d.rotateAround Axis3d.y (Angle.degrees -22.5)
        viewpoint : Viewpoint3d.Viewpoint3d Meters coordinates
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
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
            Rectangle2d.from Point2d.origin (Point2d.pixels world.galaxyViewSize.width world.galaxyViewSize.height)

        angle : Angle.Angle
        angle =
            Angle.degrees 0.0

        planetVertices2d :
            List
                { id : EntityID
                , size : Length
                , vertex : Point2d.Point2d Pixels.Pixels ScaledViewPoint
                , type_ : CelestialBodyForm
                , arc : List (LineSegment2d.LineSegment2d Pixels.Pixels coordinates)
                }
        planetVertices2d =
            List.map
                (\details ->
                    let
                        vertex : Point3d Meters ScaledViewPoint
                        vertex =
                            Point3d.rotateAround Axis3d.z
                                angle
                                (scalePointInAstroUnitsToOne details.position)
                    in
                    { id = details.id
                    , size = details.size
                    , vertex =
                        Point3d.Projection.toScreenSpace camera
                            screenRectangle
                            vertex
                    , type_ = details.type_
                    , arc =
                        Circle3d.withRadius
                            details.orbitDistance
                            Direction3d.positiveZ
                            Point3d.origin
                            |> Circle3d.toArc
                            |> Arc3d.segments (max 20 (ceiling (Length.inAstronomicalUnits details.orbitDistance) * 3))
                            |> Polyline3d.segments
                            |> List.map (LineSegment3d.Projection.toScreenSpace camera screenRectangle)
                    }
                )
                planetDetails

        planetLabels : List (Svg.Svg msg)
        planetLabels =
            List.map
                (\planet ->
                    let
                        highlightPlanet : Bool
                        highlightPlanet =
                            List.any
                                (\civId ->
                                    world.civilizationPopulations
                                        |> Logic.Component.get civId
                                        |> Maybe.map
                                            (\dictPlanetPopulatiopns ->
                                                Dict.member planet.id dictPlanetPopulatiopns && Just civId == options.focusedCivilization
                                            )
                                        |> Maybe.withDefault False
                                )
                                (Set.toList world.civilizations)
                    in
                    Svg.g
                        [ Svg.Attributes.class
                            (if highlightPlanet then
                                "galactic-label-focus-civ"

                             else if options.onPressPlanet == Nothing then
                                "galactic-label-no-show"

                             else
                                "galactic-label"
                            )
                        ]
                        [ -- Orbit highlight
                          Geometry.Svg.polyline2d
                            [ case options.onPressPlanet of
                                Nothing ->
                                    Svg.Attributes.style ""

                                Just onPressPlanet ->
                                    Svg.Events.onClick (onPressPlanet planet.id)

                            -- This isn't working, need to debug for accessibility
                            -- , Html.Attributes.tabindex 0
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.stroke "rgb(255, 255, 0)"
                            , Svg.Attributes.fill "rgba(0, 0, 255, 0)"
                            ]
                            (Polyline2d.fromVertices
                                (List.concatMap
                                    (\seg ->
                                        [ LineSegment2d.startPoint seg, LineSegment2d.endPoint seg ]
                                    )
                                    planet.arc
                                )
                            )

                        -- Planet highlight
                        , if highlightPlanet then
                            Geometry.Svg.circle2d
                                [ Svg.Attributes.stroke "rgb(0, 255, 200)"
                                , Svg.Attributes.strokeWidth "2"
                                , Svg.Attributes.fillOpacity "0"
                                ]
                                (Circle2d.withRadius
                                    (Pixels.float
                                        (Length.inKilometers planet.size
                                            / ((case planet.type_ of
                                                    Rocky ->
                                                        0.000000001

                                                    Gas ->
                                                        0.000000005
                                               )
                                                * world.zoom
                                              )
                                        )
                                    )
                                    --
                                    planet.vertex
                                )

                          else
                            Svg.text ""
                        , Geometry.Svg.lineSegment2d
                            [ Svg.Attributes.stroke "white"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDashoffset "5 5"
                            , Svg.Attributes.class "galactic-label-ignore"
                            ]
                            (LineSegment2d.from planet.vertex
                                (Point2d.pixels
                                    (Pixels.toFloat (Point2d.xCoordinate planet.vertex))
                                    (Pixels.toFloat (Point2d.yCoordinate planet.vertex) + 40)
                                )
                            )
                        , -- Hack: flip the text upside down since our later
                          -- 'Svg.relativeTo topLeftFrame' call will flip it
                          -- back right side up
                          Geometry.Svg.mirrorAcross (Axis2d.through planet.vertex Direction2d.x)
                            (Svg.text_
                                [ Svg.Attributes.fill "white"
                                , Svg.Attributes.fontSize "25px"
                                , Svg.Attributes.stroke "black"
                                , Svg.Attributes.strokeWidth "1px"
                                , Svg.Attributes.fontFamily "sans-serif"
                                , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate planet.vertex) - 40))
                                , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate planet.vertex)))
                                , Svg.Attributes.class "galactic-label-ignore"
                                ]
                                [ Svg.text ("P_" ++ String.fromInt planet.id) ]
                            )
                        ]
                )
                planetVertices2d

        starVertices2d : List ( EntityID, Temperature, Point2d.Point2d Pixels.Pixels ScaledViewPoint )
        starVertices2d =
            List.map
                (\details ->
                    ( details.id
                    , details.temperature
                    , scalePointInAstroUnitsToOne details.position
                        |> Point3d.rotateAround Axis3d.z angle
                        |> Point3d.Projection.toScreenSpace camera screenRectangle
                    )
                )
                starDetails

        starLabels : List (Svg.Svg msg)
        starLabels =
            List.map
                (\( starId, temperature, vertex ) ->
                    Svg.g
                        [ Svg.Attributes.class "galactic-label"
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(255, 255, 0)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(255, 0, 0, 0)"
                            , case options.onPressStar of
                                Nothing ->
                                    Svg.Attributes.style ""

                                Just onPressStar ->
                                    Svg.Events.onClick (onPressStar starId)

                            -- This isn't working, need to debug for accessibility
                            -- , Html.Attributes.tabindex 0
                            ]
                            (Circle2d.withRadius
                                (Pixels.float
                                    (Length.inKilometers (Data.Star.temperatureToRadius temperature) / (0.00000001 * world.zoom))
                                )
                                vertex
                            )
                        , Geometry.Svg.lineSegment2d
                            [ Svg.Attributes.stroke "white"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDasharray "5 5"
                            , Svg.Attributes.class "galactic-label-ignore"
                            ]
                            (LineSegment2d.from vertex (Point2d.pixels (world.galaxyViewSize.width / 2) (world.galaxyViewSize.height - 50)))
                        , -- Hack: flip the text upside down since our later
                          -- 'Svg.relativeTo topLeftFrame' call will flip it
                          -- back right side up
                          Geometry.Svg.mirrorAcross (Axis2d.through (Point2d.fromMeters { x = world.galaxyViewSize.width / 2, y = world.galaxyViewSize.height / 2 }) Direction2d.x)
                            (Svg.text_
                                [ Svg.Attributes.fill "white"
                                , Svg.Attributes.fontFamily "sans-serif"
                                , Svg.Attributes.fontSize "25px"
                                , Svg.Attributes.stroke "black"
                                , Svg.Attributes.x (String.fromFloat (world.galaxyViewSize.width / 2))
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
            Pixels.float world.galaxyViewSize.height
                |> Point2d.xy Quantity.zero
                |> Frame2d.atPoint
                |> Frame2d.reverseY

        -- Create an SVG element with the projected points, lines and
        -- associated labels
        solarSystemLabels : Html msg
        solarSystemLabels =
            Svg.svg
                [ Html.Attributes.width (floor world.galaxyViewSize.width)
                , Html.Attributes.height (floor world.galaxyViewSize.height)
                ]
                [ Geometry.Svg.relativeTo topLeftFrame (Svg.g [] (planetLabels ++ starLabels)) ]

        solarSystemScene : Html msg
        solarSystemScene =
            case settings.realisticLighting of
                Disabled ->
                    Scene3d.unlit
                        { entities =
                            -- DEBUG
                            -- Scene3d.quad (Material.color (Color.rgba 1 1 1 0.1))
                            --     (Point3d.meters -1500000000 -1500000000 0)
                            --     (Point3d.meters 1500000000 -1500000000 0)
                            --     (Point3d.meters 1500000000 1500000000 0)
                            --     (Point3d.meters -1500000000 1500000000 0)
                            --     ::
                            planetEntities
                                ++ starEntities
                        , camera = camera
                        , clipDepth = Length.meters 1
                        , background = Scene3d.backgroundColor Color.black
                        , dimensions =
                            ( Pixels.pixels (floor world.galaxyViewSize.width)
                            , Pixels.pixels (floor world.galaxyViewSize.height)
                            )
                        }

                Enabled ->
                    Scene3d.custom
                        { entities =
                            -- DEBUG
                            -- Scene3d.quad (Material.color (Color.rgba 1 1 1 0.1))
                            --     (Point3d.meters -1500000000 -1500000000 0)
                            --     (Point3d.meters 1500000000 -1500000000 0)
                            --     (Point3d.meters 1500000000 1500000000 0)
                            --     (Point3d.meters -1500000000 1500000000 0)
                            --     ::
                            planetEntities
                                ++ starEntities
                        , camera = camera
                        , clipDepth = Length.meters 1
                        , background = Scene3d.backgroundColor Color.black
                        , dimensions =
                            ( Pixels.pixels (floor world.galaxyViewSize.width)
                            , Pixels.pixels (floor world.galaxyViewSize.height)
                            )
                        , lights = getLightsFromStars starDetails
                        , exposure = Scene3d.maxLuminance (Luminance.nits 100000)
                        , toneMapping = Scene3d.noToneMapping

                        -- TODO: This should somehow use the colors of the stars in the scene
                        , whiteBalance = Scene3d.Light.color Color.yellow
                        , antialiasing = Scene3d.noAntialiasing
                        }
    in
    viewSpace options solarSystemLabels solarSystemScene


getLightsFromStars : List StarRenderDetails -> Scene3d.Lights ScaledViewPoint
getLightsFromStars starDetails =
    let
        lightFromStar : { a | temperature : Temperature } -> Scene3d.Light.Light coordinates Bool
        lightFromStar star =
            Scene3d.Light.point
                (Scene3d.Light.castsShadows True)
                { chromaticity = Scene3d.Light.color (Data.Star.tempuratureToColor star.temperature)
                , intensity = Data.Star.temperatureToLuminosity star.temperature
                , position = Point3d.origin
                }

        lightFromExtraStar : { a | temperature : Temperature } -> Scene3d.Light.Light coordinates Never
        lightFromExtraStar star =
            Scene3d.Light.point
                Scene3d.Light.neverCastsShadows
                { chromaticity = Scene3d.Light.color (Data.Star.tempuratureToColor star.temperature)
                , intensity = Data.Star.temperatureToLuminosity star.temperature
                , position = Point3d.origin
                }
    in
    case starDetails of
        [ starA ] ->
            Scene3d.oneLight (lightFromStar starA)

        [ starA, starB ] ->
            Scene3d.twoLights
                (lightFromStar starA)
                (lightFromStar starB)

        [ starA, starB, starC ] ->
            Scene3d.threeLights
                (lightFromStar starA)
                (lightFromStar starB)
                (lightFromStar starC)

        [ starA, starB, starC, starD ] ->
            Scene3d.fourLights
                (lightFromStar starA)
                (lightFromStar starB)
                (lightFromStar starC)
                (lightFromStar starD)

        [ starA, starB, starC, starD, starE ] ->
            Scene3d.fiveLights
                (lightFromStar starA)
                (lightFromStar starB)
                (lightFromStar starC)
                (lightFromStar starD)
                (lightFromExtraStar starE)

        [ starA, starB, starC, starD, starE, starF ] ->
            Scene3d.sixLights
                (lightFromStar starA)
                (lightFromStar starB)
                (lightFromStar starC)
                (lightFromStar starD)
                (lightFromExtraStar starE)
                (lightFromExtraStar starF)

        [ starA, starB, starC, starD, starE, starF, starG ] ->
            Scene3d.sevenLights
                (lightFromStar starA)
                (lightFromStar starB)
                (lightFromStar starC)
                (lightFromStar starD)
                (lightFromExtraStar starE)
                (lightFromExtraStar starF)
                (lightFromExtraStar starG)

        _ ->
            Scene3d.oneLight
                (Scene3d.Light.point
                    (Scene3d.Light.castsShadows True)
                    { chromaticity = Scene3d.Light.color Color.yellow

                    -- Have to break these numbers up so that they don't wrap negatively by elm-format
                    , intensity = LuminousFlux.lumens (16240000000000 * 2200000000 * 100000000)
                    , position = Point3d.origin
                    }
                )


viewSpace :
    { r
        | onZoom : Maybe (Value -> msg)
        , onZoomPress : Maybe (Float -> msg)
        , onRotationPress : Maybe (Float -> msg)
    }
    -> Html msg
    -> Html msg
    -> Html msg
viewSpace options labels scene =
    Ui.stack
        [ Ui.height.fill
        , Html.Attributes.id "galaxy-view"
        , case options.onZoom of
            Nothing ->
                Html.Attributes.classList []

            Just onZoom ->
                Html.Events.preventDefaultOn "wheel"
                    (Json.Decode.map (\v -> ( onZoom v, True ))
                        Json.Decode.value
                    )
        ]
        [ scene
        , Ui.row
            [ Ui.justifySelf.end
            , Ui.alignSelf.end
            , Ui.padding.rem1
            , Ui.gap.remHalf
            , Ui.width.shrink
            ]
            [ Ui.row
                [ Ui.alignSelf.end
                , Ui.gap.remHalf
                ]
                [ case options.onRotationPress of
                    Nothing ->
                        Ui.none

                    Just onRotationPress ->
                        Ui.Button.default []
                            { onPress = Just (onRotationPress -5)
                            , label = Ui.text "<-"
                            }
                , case options.onRotationPress of
                    Nothing ->
                        Ui.none

                    Just onRotationPress ->
                        Ui.Button.default []
                            { onPress = Just (onRotationPress 5)
                            , label = Ui.text "->"
                            }
                ]
            , Ui.column [ Ui.gap.remHalf ]
                [ case options.onZoomPress of
                    Nothing ->
                        Ui.none

                    Just onZoomPress ->
                        Ui.Button.default []
                            { onPress = Just (onZoomPress -10.0)
                            , label = Ui.text "+"
                            }
                , case options.onZoomPress of
                    Nothing ->
                        Ui.none

                    Just onZoomPress ->
                        Ui.Button.default []
                            { onPress = Just (onZoomPress 10.0)
                            , label = Ui.text "-"
                            }
                ]
            ]
        ]



-- Helpers


renderSolarSystem : Point3d Meters LightYear -> Scene3d.Entity ScaledViewPoint
renderSolarSystem position =
    Scene3d.sphere
        (Material.color Color.gray)
        (Sphere3d.atPoint (scalePointInLightYearsToOne position) (Length.lightYears 300))


type ScaledViewPoint
    = ScaledViewPoint Never


scalePointInLightYearsToOne : Point3d Meters LightYear -> Point3d Meters ScaledViewPoint
scalePointInLightYearsToOne point =
    Point3d.fromMeters
        { x = Length.inMeters (Point3d.xCoordinate point)
        , y = Length.inMeters (Point3d.yCoordinate point)
        , z = Length.inMeters (Point3d.zCoordinate point)
        }


renderPlanet : Settings -> PlanetRenderDetails -> List (Scene3d.Entity ScaledViewPoint)
renderPlanet settings details =
    let
        planetEntity : Scene3d.Entity ScaledViewPoint
        planetEntity =
            case settings.realisticLighting of
                Disabled ->
                    details.size
                        |> Quantity.multiplyBy 1000
                        |> Sphere3d.atPoint (scalePointInAstroUnitsToOne details.position)
                        |> Scene3d.sphere (Material.color details.color)

                Enabled ->
                    details.size
                        |> Quantity.multiplyBy 1000
                        |> Sphere3d.atPoint (scalePointInAstroUnitsToOne details.position)
                        |> Scene3d.sphereWithShadow (Material.matte details.color)
    in
    case settings.showPlanetsOrbit of
        Enabled ->
            [ planetEntity
            , let
                segments : number
                segments =
                    100

                radius : Float
                radius =
                    Length.inMeters (Point3d.distanceFrom Point3d.origin details.position)

                t : Int -> Float
                t index =
                    2 * pi / segments * toFloat index

                verts : List (Point3d Meters ScaledViewPoint)
                verts =
                    List.map
                        (\index ->
                            scalePointInAstroUnitsToOne
                                (Point3d.meters
                                    (radius * cos (t index))
                                    (radius * sin (t index))
                                    0
                                )
                        )
                        (List.range 0 segments)
              in
              Scene3d.mesh
                (Material.color Color.gray)
                (Scene3d.Mesh.polyline (Polyline3d.fromVertices verts))
            ]

        Disabled ->
            [ planetEntity ]


scalePointInAstroUnitsToOne : Point3d Meters AstronomicalUnit -> Point3d Meters ScaledViewPoint
scalePointInAstroUnitsToOne point =
    Point3d.fromMeters
        { x = Length.inMeters (Point3d.xCoordinate point)
        , y = Length.inMeters (Point3d.yCoordinate point)
        , z = Length.inMeters (Point3d.zCoordinate point)
        }


type alias PlanetRenderDetails =
    { id : EntityID
    , color : Color.Color
    , position : Point3d Meters AstronomicalUnit
    , size : Length
    , type_ : CelestialBodyForm
    , orbitDistance : Length
    }


getPlanetDetails : Settings -> MinRenderableWorld r -> EntityID -> Maybe PlanetRenderDetails
getPlanetDetails settings world planetId =
    Maybe.map3
        (\orbit planetType_ waterPercent ->
            { id = planetId
            , orbitDistance = Data.Orbit.distance orbit
            , position =
                let
                    initialPoint : Point3d Meters coordinates
                    initialPoint =
                        Point3d.fromMeters
                            { x = 0
                            , y = Length.inMeters (Data.Orbit.distance orbit)
                            , z = 0
                            }
                in
                case settings.planetsOrbit of
                    Enabled ->
                        Point3d.rotateAround
                            Axis3d.z
                            (Angle.degrees
                                ((world.elapsedTime / Data.EarthYear.inEarthYears (Data.Orbit.period orbit))
                                    * Percent.toFloat settings.planetRotationSpeed
                                )
                            )
                            initialPoint

                    Disabled ->
                        initialPoint
            , color =
                case planetType_ of
                    Rocky ->
                        if Quantity.lessThan (Percent.fromFloat 0.5) waterPercent then
                            Color.brown

                        else
                            Color.blue

                    Gas ->
                        if Quantity.lessThan (Percent.fromFloat 0.5) waterPercent then
                            Color.lightGreen

                        else
                            Color.lightOrange
            , size =
                case planetType_ of
                    Rocky ->
                        Length.kilometers 6371

                    Gas ->
                        Length.kilometers 69911
            , type_ = planetType_
            }
        )
        (Logic.Component.get planetId world.orbits)
        (Logic.Component.get planetId world.planetTypes)
        (Logic.Component.get planetId world.waterContent)


renderStar : Settings -> StarRenderDetails -> Scene3d.Entity ScaledViewPoint
renderStar settings details =
    let
        color : Color.Color
        color =
            Data.Star.tempuratureToColor details.temperature
    in
    Scene3d.sphere
        (case settings.realisticLighting of
            Enabled ->
                Material.emissive (Scene3d.Light.color color) (Luminance.nits 100000)

            Disabled ->
                Material.color color
        )
        (details.temperature
            |> Data.Star.temperatureToRadius
            |> Quantity.multiplyBy 500
            |> Sphere3d.atPoint (scalePointInAstroUnitsToOne details.position)
        )


type alias StarRenderDetails =
    { id : EntityID
    , temperature : Temperature
    , position : Point3d Meters AstronomicalUnit
    }


getStarDetails : MinRenderableWorld r -> EntityID -> Maybe StarRenderDetails
getStarDetails world starId =
    Maybe.map
        (\temperature ->
            { id = starId
            , temperature = temperature
            , position =
                Point3d.fromMeters
                    { x = 0
                    , y = 0
                    , z = 0
                    }
            }
        )
        (Logic.Component.get starId world.starTemperature)


getGalaxyViewport : (Result Browser.Dom.Error Viewport -> msg) -> SubCmd msg effect
getGalaxyViewport gotViewport =
    SubCmd.cmd
        (Task.attempt gotViewport
            (Task.andThen
                (\() ->
                    Browser.Dom.getViewportOf "galaxy-view"
                )
                (Process.sleep 100)
            )
        )
