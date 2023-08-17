module Route exposing
    ( GenerationConfig
    , PlayType(..)
    , Route(..)
    , fromUrl
    , toString
    )

import AppUrl
import CubicSpline2d exposing (CubicSpline2d)
import Data.Name exposing (Name)
import Dict
import List.Nonempty exposing (Nonempty)
import Scalable exposing (Scalable)
import Serialize exposing (Codec)
import Url


type Route
    = Home
    | NewGameParticipate
    | NewGameObserve
    | Playing GenerationConfig


fromUrl : Url.Url -> Route
fromUrl url =
    let
        appUrl : AppUrl.AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [] ->
            Home

        [ "newgame", "participate" ] ->
            NewGameParticipate

        [ "newgame", "observe" ] ->
            NewGameObserve

        [ "playing" ] ->
            case Dict.get "config" appUrl.queryParameters of
                Just [ config ] ->
                    case Serialize.decodeFromString generationConfigCodec config of
                        Ok generationConfig ->
                            Playing generationConfig

                        Err _ ->
                            Home

                _ ->
                    Home

        _ ->
            Home


toString : Route -> String
toString route =
    (case route of
        Home ->
            { path = []
            , queryParameters = Dict.empty
            , fragment = Nothing
            }

        NewGameParticipate ->
            { path = [ "newgame", "participate" ]
            , queryParameters = Dict.empty
            , fragment = Nothing
            }

        NewGameObserve ->
            { path = [ "newgame", "observe" ]
            , queryParameters = Dict.empty
            , fragment = Nothing
            }

        Playing generationConfig ->
            { path = [ "playing" ]
            , queryParameters = Dict.singleton "config" [ Serialize.encodeToString generationConfigCodec generationConfig ]
            , fragment = Nothing
            }
    )
        |> AppUrl.toString


type alias GenerationConfig =
    { minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
    , playerStuff : Maybe PlayerStuff
    }


type Game
    = Game Never


generationConfigCodec : Codec e GenerationConfig
generationConfigCodec =
    Serialize.record
        (\playerStuff minSolarSystemsToGenerate maxSolarSystemsToGenerate minPlanetsPerSolarSystemToGenerate maxPlanetsPerSolarSystemToGenerate starCounts ->
            { minSolarSystemsToGenerate = minSolarSystemsToGenerate
            , maxSolarSystemsToGenerate = maxSolarSystemsToGenerate
            , minPlanetsPerSolarSystemToGenerate = minPlanetsPerSolarSystemToGenerate
            , maxPlanetsPerSolarSystemToGenerate = maxPlanetsPerSolarSystemToGenerate
            , starCounts = starCounts
            , playerStuff = playerStuff
            }
        )
        |> Serialize.field .playerStuff (Serialize.maybe playerStuffCodec)
        |> Serialize.field .minSolarSystemsToGenerate Serialize.int
        |> Serialize.field .maxSolarSystemsToGenerate Serialize.int
        |> Serialize.field .minPlanetsPerSolarSystemToGenerate Serialize.int
        |> Serialize.field .maxPlanetsPerSolarSystemToGenerate Serialize.int
        |> Serialize.field .starCounts (List.Nonempty.codec (Serialize.tuple Serialize.float Serialize.int))
        |> Serialize.finishRecord


type alias PlayerStuff =
    { name : Name
    , homePlanetName : String
    , reproductionMotivation : Scalable
    }


playerStuffCodec : Codec e PlayerStuff
playerStuffCodec =
    Serialize.record
        (\name homePlanetName reproductionMotivation ->
            { name = Data.Name.fromString name
            , homePlanetName = homePlanetName
            , reproductionMotivation = reproductionMotivation
            }
        )
        |> Serialize.field (\rec -> Data.Name.toString rec.name) Serialize.string
        |> Serialize.field .homePlanetName Serialize.string
        |> Serialize.field .reproductionMotivation Scalable.codec
        |> Serialize.finishRecord


type PlayType
    = Observation
    | Participation


playTypeCodec : Codec e PlayType
playTypeCodec =
    Serialize.customType
        (\observationEncoder participationEncoder value ->
            case value of
                Observation ->
                    observationEncoder

                Participation ->
                    participationEncoder
        )
        |> Serialize.variant0 Observation
        |> Serialize.variant0 Participation
        |> Serialize.finishCustomType
