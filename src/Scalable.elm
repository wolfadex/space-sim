module Scalable exposing
    ( Scalable
    , adjustInputBy
    , codec
    , getOutput
    , new
    , setInput
    )

import CubicSpline2d exposing (CubicSpline2d)
import Point2d
import Polyline2d
import Quantity exposing (Unitless)
import Serialize exposing (Codec)


codec : Codec e Scalable
codec =
    Serialize.record
        (\spline input min_ max_ ->
            { spline = spline
            , input = input
            , min = min_
            , max = max_
            }
        )
        |> Serialize.field .spline (Serialize.list (Serialize.tuple Serialize.float Serialize.float))
        |> Serialize.field .input Serialize.float
        |> Serialize.field .min Serialize.float
        |> Serialize.field .max Serialize.float
        |> Serialize.finishRecord
        |> Serialize.map Scalable (\(Scalable scalable) -> scalable)


type Scalable
    = Scalable
        { spline : List ( Float, Float )
        , input : Float
        , min : Float
        , max : Float
        }


new :
    { spline : CubicSpline2d Unitless coordinates
    , initialInput : Float
    , min : Float
    , max : Float
    }
    -> Scalable
new config =
    Scalable
        { spline =
            config.spline
                |> CubicSpline2d.segments 100
                |> Polyline2d.vertices
                |> List.map
                    (\point ->
                        Point2d.toTuple
                            Quantity.unwrap
                            point
                    )
                |> List.sortBy Tuple.first
        , input = config.initialInput
        , min = config.min
        , max = config.max
        }


adjustInputBy : Float -> Scalable -> Scalable
adjustInputBy amt (Scalable scalable) =
    Scalable
        { scalable
            | input =
                (scalable.input + amt)
                    |> min scalable.max
                    |> max scalable.min
        }


setInput : Float -> Scalable -> Scalable
setInput input (Scalable scalable) =
    Scalable
        { scalable
            | input =
                input
                    |> min scalable.max
                    |> max scalable.min
        }


getOutput : Scalable -> Float
getOutput (Scalable scalable) =
    scalable.spline
        -- given an `input` of 0.5
        -- and points of [ ( 0, 0 ), ( 0.25, 0.25 ), ( 0.5, 0.5 ), ( 0.75, 0.75 ), ( 1.0, 1.0 ) ]
        -- this sort returns [ ( 0, 0.5 ), ( 0.25, 0.25 ), ( 0.25, 0.75 ), ( 0.5, 0 ), ( 0.5, 1.0 ) ]
        |> List.sortBy
            (\( x, _ ) ->
                abs (x - scalable.input)
            )
        -- then we take the first, aka the closest to our input
        |> List.head
        -- pull out the y value
        |> Maybe.map Tuple.second
        -- default to 0, cause it's a game and why not
        |> Maybe.withDefault 0
