module Data.Star exposing
    ( random
    , temperatureSpec
    , temperatureToLuminosity
    , temperatureToRadius
    , tempuratureToColor
    )

{-| Most of this is derived from <http://www.atlasoftheuniverse.com/startype.html>
-}

import Color exposing (Color)
import Length exposing (Length)
import Logic.Component exposing (Spec)
import LuminousFlux exposing (LuminousFlux)
import Quantity
import Random exposing (Generator)
import Temperature exposing (Temperature)


tempuratureToColor : Temperature -> Color
tempuratureToColor temp =
    let
        tempInKelvins : Float
        tempInKelvins =
            Temperature.inKelvins temp
    in
    if tempInKelvins > 33000 then
        Color.blue

    else if tempInKelvins >= 10000 then
        Color.lightBlue

    else if tempInKelvins >= 7500 then
        Color.white

    else if tempInKelvins >= 6000 then
        Color.lightYellow

    else if tempInKelvins >= 5200 then
        Color.yellow

    else if tempInKelvins >= 3700 then
        Color.orange

    else
        Color.red


temperatureToRadius : Temperature -> Length
temperatureToRadius temp =
    let
        tempInKelvins : Float
        tempInKelvins =
            Temperature.inKelvins temp

        baseRadius : Length
        baseRadius =
            Length.kilometers 695700
    in
    Quantity.multiplyBy
        (if tempInKelvins > 33000 then
            10

         else if tempInKelvins >= 10000 then
            5

         else if tempInKelvins >= 7500 then
            1.7

         else if tempInKelvins >= 6000 then
            1.3

         else if tempInKelvins >= 5200 then
            1.0

         else if tempInKelvins >= 3700 then
            0.8

         else
            0.3
        )
        baseRadius


temperatureToLuminosity : Temperature -> LuminousFlux
temperatureToLuminosity temp =
    let
        baseLuminosity : LuminousFlux
        baseLuminosity =
            -- Have to break these numbers up so that they don't wrap negatively by elm-format
            LuminousFlux.lumens (16240000000000 * 2200000000 * 100000000)

        tempInKelvins : Float
        tempInKelvins =
            Temperature.inKelvins temp
    in
    Quantity.multiplyBy
        (if tempInKelvins > 33000 then
            100000.0

         else if tempInKelvins >= 10000 then
            1000.0

         else if tempInKelvins >= 7500 then
            20.0

         else if tempInKelvins >= 6000 then
            4.0

         else if tempInKelvins >= 5200 then
            1.0

         else if tempInKelvins >= 3700 then
            0.2

         else
            0.01
        )
        baseLuminosity


{-| The temperature range is weighted based on abundance, and then a random temp for that abundance's range is generated
-}
random : Generator Temperature
random =
    Random.andThen (Random.map Temperature.kelvins)
        (Random.weighted
            ( 0.00001, Random.float 33000 42000 )
            [ ( 0.1, Random.float 10000 32999 )
            , ( 0.7, Random.float 7500 19999 )
            , ( 2.0, Random.float 6000 7499 )
            , ( 3.5, Random.float 5200 6999 )
            , ( 8.0, Random.float 3700 5199 )
            , ( 80.0, Random.float 3200 3699 )
            ]
        )


temperatureSpec : Spec Temperature { world | starTemperature : Logic.Component.Set Temperature }
temperatureSpec =
    Logic.Component.Spec .starTemperature (\comps world -> { world | starTemperature = comps })
