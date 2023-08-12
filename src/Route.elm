module Route exposing
    ( GenerationConfig
    , PlayType(..)
    , Route(..)
    , fromUrl
    , toString
    )

import AppUrl
import Data.Name exposing (Name)
import Dict
import List.Nonempty exposing (Nonempty)
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
    { name : Name
    , homePlanetName : String
    , minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
    , playType : PlayType
    }


generationConfigCodec : Codec e GenerationConfig
generationConfigCodec =
    Serialize.record
        (\name homePlanetName minSolarSystemsToGenerate maxSolarSystemsToGenerate minPlanetsPerSolarSystemToGenerate maxPlanetsPerSolarSystemToGenerate starCounts playType ->
            { name = Data.Name.fromString name
            , homePlanetName = homePlanetName
            , minSolarSystemsToGenerate = minSolarSystemsToGenerate
            , maxSolarSystemsToGenerate = maxSolarSystemsToGenerate
            , minPlanetsPerSolarSystemToGenerate = minPlanetsPerSolarSystemToGenerate
            , maxPlanetsPerSolarSystemToGenerate = maxPlanetsPerSolarSystemToGenerate
            , starCounts = starCounts
            , playType = playType
            }
        )
        |> Serialize.field (\rec -> Data.Name.toString rec.name) Serialize.string
        |> Serialize.field .homePlanetName Serialize.string
        |> Serialize.field .minSolarSystemsToGenerate Serialize.int
        |> Serialize.field .maxSolarSystemsToGenerate Serialize.int
        |> Serialize.field .minPlanetsPerSolarSystemToGenerate Serialize.int
        |> Serialize.field .maxPlanetsPerSolarSystemToGenerate Serialize.int
        |> Serialize.field .starCounts (List.Nonempty.codec (Serialize.tuple Serialize.float Serialize.int))
        |> Serialize.field .playType playTypeCodec
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
