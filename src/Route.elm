module Route exposing (Route(..), fromUrl, toString)

import AppUrl
import Dict
import Serialize
import Shared
import Url


type Route
    = Home
    | NewGameParticipate
    | NewGameObserve
    | Playing Shared.GenerationConfig


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
                    case Serialize.decodeFromString Shared.generationConfigCodec config of
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
            , queryParameters = Dict.singleton "config" [ Serialize.encodeToString Shared.generationConfigCodec generationConfig ]
            , fragment = Nothing
            }
    )
        |> AppUrl.toString
