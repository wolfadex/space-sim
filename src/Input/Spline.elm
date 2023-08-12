module Input.Spline exposing (Config(..), Internal, Model(..), Msg(..), Options, init, initWith, internalScale, limit, mapPoint2d, new, parse, pointerDown, pointerMove, pointerUp, scaleWithin, splineControl, toControl, update, view, withUnits)

import Circle2d
import Control exposing (Control)
import CubicSpline2d exposing (CubicSpline2d)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Svg
import Svg.Attributes
import Svg.Events
import Vector2d


new : { xMin : Float, xMax : Float, yMin : Float, yMax : Float } -> Config
new options =
    Config
        { xMin = options.xMin
        , xMax = options.xMax
        , yMin = options.yMin
        , yMax = options.yMax
        , units = Nothing
        }


withUnits : String -> Config -> Config
withUnits units (Config options) =
    Config
        { options
            | units = Just units
        }


toControl : Config -> Control (Model coordinates) Msg (CubicSpline2d Pixels coordinates)
toControl (Config options) =
    splineControl options


type Config
    = Config Options


splineControl : Options -> Control (Model coordinates) Msg (CubicSpline2d Pixels coordinates)
splineControl options =
    Control.create
        { label = "Spline"
        , initEmpty = ( init options, Cmd.none )
        , initWith = \spline -> ( initWith options spline, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , parse = parse
        }


type alias Options =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    , units : Maybe String
    }


parse : Model coordinates -> Result (List String) (CubicSpline2d Pixels coordinates)
parse (Model model) =
    let
        scaleToConfig : Float -> Float
        scaleToConfig =
            scaleWithin
                { out = { min = model.options.xMin, max = model.options.xMax }
                , in_ = internalScale
                }

        parsePoint : { x : Float, y : Float } -> { x : Float, y : Float }
        parsePoint { x, y } =
            { x = scaleToConfig x, y = model.options.yMax - scaleToConfig y }
    in
    Ok
        (CubicSpline2d.fromControlPoints
            (model.firstControlPoint
                |> mapPoint2d parsePoint
            )
            (model.secondControlPoint
                |> mapPoint2d parsePoint
            )
            (model.thirdControlPoint
                |> mapPoint2d parsePoint
            )
            (model.fourthControlPoint
                |> mapPoint2d parsePoint
            )
        )


type Model coordinates
    = Model (Internal coordinates)


type alias Internal coordinates =
    { firstControlPoint : Point2d Pixels coordinates
    , secondControlPoint : Point2d Pixels coordinates
    , thirdControlPoint : Point2d Pixels coordinates
    , fourthControlPoint : Point2d Pixels coordinates
    , pointerDown :
        Maybe
            { pointerId : Json.Decode.Value
            , index : Int
            , point : Point2d Pixels coordinates
            }
    , options : Options
    }


init : Options -> Model coordinates
init options =
    Model
        { firstControlPoint = Point2d.pixels 5 95
        , secondControlPoint = Point2d.pixels 45 95
        , thirdControlPoint = Point2d.pixels 55 5
        , fourthControlPoint = Point2d.pixels 95 5
        , pointerDown = Nothing
        , options = options
        }


initWith : Options -> CubicSpline2d Pixels coordinates -> Model coordinates
initWith options spline =
    let
        scaleFromConfig : Float -> Float
        scaleFromConfig =
            scaleWithin
                { out = internalScale
                , in_ = { min = options.xMin, max = options.xMax }
                }

        initPoint : { x : Float, y : Float } -> { x : Float, y : Float }
        initPoint { x, y } =
            { x = scaleFromConfig x, y = internalScale.max - scaleFromConfig y }
    in
    Model
        { firstControlPoint =
            CubicSpline2d.firstControlPoint spline
                |> mapPoint2d initPoint
        , secondControlPoint =
            CubicSpline2d.secondControlPoint spline
                |> mapPoint2d initPoint
        , thirdControlPoint =
            CubicSpline2d.thirdControlPoint spline
                |> mapPoint2d initPoint
        , fourthControlPoint =
            CubicSpline2d.fourthControlPoint spline
                |> mapPoint2d initPoint
        , pointerDown = Nothing
        , options = options
        }


mapPoint2d : ({ x : Float, y : Float } -> { x : Float, y : Float }) -> Point2d units coordinates -> Point2d units coordinates
mapPoint2d fn point =
    point
        |> Point2d.unwrap
        |> fn
        |> Point2d.unsafe


scaleWithin :
    { out : { min : Float, max : Float }
    , in_ : { min : Float, max : Float }
    }
    -> Float
    -> Float
scaleWithin { out, in_ } input =
    ((input - in_.min) / (in_.max - in_.min)) * (out.max - out.min) + out.min


type Msg
    = PointerDown Int Json.Decode.Value Float Float
    | PointerMove Int Float Float
    | PointerUp Int


update : Msg -> Model coordinates -> ( Model coordinates, Cmd Msg )
update msg (Model model) =
    case msg of
        PointerDown index pointerId x y ->
            ( Model
                { model
                    | pointerDown =
                        Just
                            { pointerId = pointerId
                            , index = index
                            , point = Point2d.pixels x y
                            }
                }
            , Cmd.none
            )

        PointerMove index x y ->
            case model.pointerDown of
                Just details ->
                    if details.index == index then
                        let
                            newPoint =
                                Point2d.pixels x y
                        in
                        ( Model
                            { model
                                | pointerDown =
                                    Just
                                        { details
                                            | point = newPoint
                                        }
                                , firstControlPoint =
                                    if index == 0 then
                                        model.firstControlPoint
                                            |> Point2d.translateBy
                                                (Vector2d.from details.point newPoint)

                                    else
                                        model.firstControlPoint
                                , secondControlPoint =
                                    if index == 1 then
                                        model.secondControlPoint
                                            |> Point2d.translateBy
                                                (Vector2d.from details.point newPoint)

                                    else
                                        model.secondControlPoint
                                , thirdControlPoint =
                                    if index == 2 then
                                        model.thirdControlPoint
                                            |> Point2d.translateBy
                                                (Vector2d.from details.point newPoint)

                                    else
                                        model.thirdControlPoint
                                , fourthControlPoint =
                                    if index == 3 then
                                        model.fourthControlPoint
                                            |> Point2d.translateBy
                                                (Vector2d.from details.point newPoint)

                                    else
                                        model.fourthControlPoint
                            }
                        , Cmd.none
                        )

                    else
                        ( Model model, Cmd.none )

                Nothing ->
                    ( Model model, Cmd.none )

        PointerUp index ->
            ( Model <|
                case model.pointerDown of
                    Just details ->
                        if details.index == index then
                            { model
                                | pointerDown = Nothing
                                , firstControlPoint = limit model.firstControlPoint
                                , secondControlPoint = limit model.secondControlPoint
                                , thirdControlPoint = limit model.thirdControlPoint
                                , fourthControlPoint = limit model.fourthControlPoint
                            }

                        else
                            model

                    Nothing ->
                        model
            , Cmd.none
            )


limit : Point2d units coordinates -> Point2d units coordinates
limit point =
    point
        |> Point2d.unwrap
        |> (\{ x, y } ->
                { x = max internalScale.min (min internalScale.max x)
                , y = max internalScale.min (min internalScale.max y)
                }
           )
        |> Point2d.unsafe


view :
    { state : Model coordinates
    , label : String
    , id : String
    , name : String
    , class : String
    }
    -> List (Html Msg)
view { state, label, id, name, class } =
    let
        (Model model) =
            state

        spline =
            CubicSpline2d.fromControlPoints
                model.firstControlPoint
                model.secondControlPoint
                model.thirdControlPoint
                model.fourthControlPoint

        controlPoints =
            [ model.firstControlPoint
            , model.secondControlPoint
            , model.thirdControlPoint
            , model.fourthControlPoint
            ]

        drawPoint index point =
            Circle2d.withRadius (Pixels.pixels 4) point
                |> Geometry.Svg.circle2d
                    ([ Svg.Attributes.cursor "pointer"
                     , Svg.Attributes.strokeWidth "1"
                     , Svg.Attributes.pointerEvents "all"
                     , Svg.Events.on "pointerdown" (pointerDown index)
                     , Svg.Events.on "pointerup" (pointerUp index)
                     ]
                        ++ (case model.pointerDown of
                                Just details ->
                                    if details.index == index then
                                        [ Svg.Attributes.fill "red"
                                        , Html.Attributes.property "___capturePointer" details.pointerId
                                        , Svg.Events.on "pointermove" (pointerMove index)
                                        ]

                                    else
                                        []

                                Nothing ->
                                    []
                           )
                    )
    in
    [ Html.div
        [ Html.Attributes.id id
        , Html.Attributes.class class
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "1rem"
        ]
        [ Html.label
            [ Html.Attributes.for name ]
            [ Html.text label ]
        , Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "gap" "0.1rem"
            , Html.Attributes.style "grid-template-columns" "auto auto"
            , Html.Attributes.style "grid-template-rows" "auto auto"
            ]
            [ Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                , Html.Attributes.style "justify-content" "space-between"
                ]
                [ Html.span [] [ Html.text (String.fromFloat model.options.yMax) ]
                , Html.span [] [ Html.text (String.fromFloat model.options.yMin) ]
                ]
            , Svg.svg
                [ Svg.Attributes.width (String.fromFloat internalScale.max)
                , Svg.Attributes.height (String.fromFloat internalScale.max)
                , Svg.Attributes.viewBox
                    ([ internalScale.min
                     , internalScale.min
                     , internalScale.max
                     , internalScale.max
                     ]
                        |> List.map String.fromFloat
                        |> String.join " "
                    )
                , Svg.Attributes.style "border: 1px solid black"
                ]
                [ Svg.g [ Svg.Attributes.stroke "blue" ]
                    [ Geometry.Svg.cubicSpline2d
                        [ Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.pointerEvents "none"
                        ]
                        spline
                    , Geometry.Svg.polyline2d
                        [ Svg.Attributes.strokeWidth "1"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeDasharray "3 3"
                        , Svg.Attributes.pointerEvents "none"
                        ]
                        (Polyline2d.fromVertices controlPoints)
                    , Svg.g [ Svg.Attributes.fill "white" ]
                        (List.indexedMap drawPoint controlPoints)
                    ]
                ]
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "space-between"
                , Html.Attributes.style "grid-column" "2"
                ]
                [ Html.span [] [ Html.text (String.fromFloat model.options.yMin) ]
                , Html.span [] [ Html.text (String.fromFloat model.options.yMax) ]
                ]
            ]
        ]
    ]


pointerDown : Int -> Json.Decode.Decoder Msg
pointerDown index =
    Json.Decode.map3
        (\pointerId clientX clientY ->
            PointerDown index pointerId clientX clientY
        )
        (Json.Decode.field "pointerId" Json.Decode.value)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


pointerUp : Int -> Json.Decode.Decoder Msg
pointerUp index =
    Json.Decode.succeed (PointerUp index)


pointerMove : Int -> Json.Decode.Decoder Msg
pointerMove index =
    Json.Decode.map2
        (\clientX clientY ->
            PointerMove index clientX clientY
        )
        (Json.Decode.at [ "clientX" ] Json.Decode.float)
        (Json.Decode.at [ "clientY" ] Json.Decode.float)


internalScale : { min : Float, max : Float }
internalScale =
    { min = 0, max = 100 }
