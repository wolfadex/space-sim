module Shared exposing
    ( Effect(..)
    , Enabled(..)
    , Flags
    , GenerationConfig
    , PlayType(..)
    , Settings
    , SharedModel
    , SharedMsg(..)
    , defaultSettings
    , encodeSettings
    , generationConfigCodec
    , init
    , update
    , viewSettings
    )

import Data.Name exposing (Name)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import List.Nonempty exposing (Nonempty)
import Percent exposing (Percent)
import Random exposing (Seed)
import Serialize exposing (Codec)
import Ui.Slider
import Ui.Theme



-- import WebAudio


init : Flags -> SharedModel
init flags =
    { seed = Random.initialSeed flags.initialSeed
    , settings =
        case decodeSettings flags.settings of
            Err _ ->
                defaultSettings

            Ok settings ->
                settings
    }


type alias Flags =
    { initialSeed : Int
    , settings : Value
    }


type alias SharedModel =
    { settings : Settings
    , seed : Seed
    }


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


type Effect
    = DeleteGame
      -- | CreateGame PlayType GenerationConfig
    | UpdateSeed Seed
    | GotSharedMessage SharedMsg



-- | PlayAudio (List WebAudio.Node)


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


type SharedMsg
    = GotLightingChange Bool
    | GotPlanetOrbitChange Bool
    | GotShowPlanetOrbitChange Bool
    | GotPlanetRotationSpeed Float



---- SETTINGS ----


update : SharedMsg -> SharedModel -> SharedModel
update msg ({ settings } as model) =
    case msg of
        GotLightingChange enabledBool ->
            { model
                | settings =
                    { settings
                        | realisticLighting =
                            if enabledBool then
                                Enabled

                            else
                                Disabled
                    }
            }

        GotPlanetOrbitChange enabledBool ->
            { model
                | settings =
                    { settings
                        | planetsOrbit =
                            if enabledBool then
                                Enabled

                            else
                                Disabled
                    }
            }

        GotShowPlanetOrbitChange enabledBool ->
            { model
                | settings =
                    { settings
                        | showPlanetsOrbit =
                            if enabledBool then
                                Enabled

                            else
                                Disabled
                    }
            }

        GotPlanetRotationSpeed speed ->
            { model
                | settings =
                    { settings
                        | planetRotationSpeed = Percent.fromFloat speed
                    }
            }


type alias Settings =
    { version : Int
    , realisticLighting : Enabled
    , planetsOrbit : Enabled
    , planetRotationSpeed : Percent ()
    , showPlanetsOrbit : Enabled
    }


defaultSettings : Settings
defaultSettings =
    { version = 0
    , realisticLighting = Enabled
    , planetsOrbit = Enabled
    , planetRotationSpeed = Percent.fromFloat 0.2
    , showPlanetsOrbit = Enabled
    }


encodeSettings : Settings -> Value
encodeSettings settings =
    Json.Encode.object
        [ ( "version", Json.Encode.int settings.version )
        , ( "realisticLighting", encodeEnabled settings.realisticLighting )
        , ( "planetsOrbit", encodeEnabled settings.planetsOrbit )
        , ( "planetRotationSpeed", Json.Encode.float (Percent.toFloat settings.planetRotationSpeed) )
        , ( "showPlanetsOrbit", encodeEnabled settings.showPlanetsOrbit )
        ]


decodeSettings : Value -> Result Json.Decode.Error Settings
decodeSettings =
    Json.Decode.decodeValue decodeSettingsInternal


decodeSettingsInternal : Decoder Settings
decodeSettingsInternal =
    Json.Decode.oneOf
        [ Json.Decode.andThen
            (\version ->
                case version of
                    0 ->
                        decodeSettings0 version

                    _ ->
                        Json.Decode.fail
                            ("Tried to load an unsupported version of settings: "
                                ++ String.fromInt version
                                ++ ". This version of the app only supports version 0."
                            )
            )
            (Json.Decode.field "version" Json.Decode.int)
        , Json.Decode.null defaultSettings
        ]


decodeSettings0 : Int -> Decoder Settings
decodeSettings0 version =
    Json.Decode.map4
        (\realisticLighting planetsOrbit planetRotationSpeed showPlanetsOrbit ->
            { version = version
            , realisticLighting = realisticLighting
            , planetsOrbit = planetsOrbit
            , planetRotationSpeed = Percent.fromFloat planetRotationSpeed
            , showPlanetsOrbit = showPlanetsOrbit
            }
        )
        (Json.Decode.field "realisticLighting" decodeEnabled)
        (Json.Decode.field "planetsOrbit" decodeEnabled)
        (Json.Decode.field "planetRotationSpeed" Json.Decode.float)
        (Json.Decode.field "showPlanetsOrbit" decodeEnabled)


type Enabled
    = Enabled
    | Disabled


encodeEnabled : Enabled -> Value
encodeEnabled enabled =
    Json.Encode.bool
        (case enabled of
            Enabled ->
                True

            Disabled ->
                False
        )


decodeEnabled : Decoder Enabled
decodeEnabled =
    Json.Decode.map
        (\enabled ->
            if enabled then
                Enabled

            else
                Disabled
        )
        Json.Decode.bool


viewSettings : Settings -> Element SharedMsg
viewSettings settings =
    el
        [ alignRight
        , moveDown 44
        , padding 16
        ]
        (column
            [ Background.color Ui.Theme.nearlyWhite
            , padding 16
            , Border.solid
            , Border.color Ui.Theme.darkGray
            , Border.width 3
            , Border.rounded 8
            , spacing 8
            ]
            [ el
                [ Font.size 30
                , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
                , Border.solid
                , width fill
                ]
                (text "Settings")
            , Input.checkbox
                []
                { onChange = GotLightingChange
                , icon = Input.defaultCheckbox
                , label = Input.labelLeft [] (text "Realistic Lighting:")
                , checked =
                    case settings.realisticLighting of
                        Enabled ->
                            True

                        Disabled ->
                            False
                }
            , Input.checkbox
                []
                { onChange = GotPlanetOrbitChange
                , icon = Input.defaultCheckbox
                , label = Input.labelLeft [] (text "Planets Orbit:")
                , checked =
                    case settings.planetsOrbit of
                        Enabled ->
                            True

                        Disabled ->
                            False
                }
            , Ui.Slider.float
                []
                { step = Just 0.01
                , min = 0.01
                , max = 1.0
                , value = Percent.toFloat settings.planetRotationSpeed
                , label = Input.labelAbove [] (text "Planet Rotation Speed")
                , onChange = GotPlanetRotationSpeed
                }
            , Input.checkbox
                []
                { onChange = GotShowPlanetOrbitChange
                , icon = Input.defaultCheckbox
                , label = Input.labelLeft [] (text "Show Planets Orbit's:")
                , checked =
                    case settings.showPlanetsOrbit of
                        Enabled ->
                            True

                        Disabled ->
                            False
                }
            , paragraph
                [ Font.size 16
                , Font.color Ui.Theme.nearlyWhite
                , Background.color Ui.Theme.darkGray
                , padding 4
                , Border.rounded 4
                ]
                [ text "For issues or to follow development, checkout the "
                , newTabLink
                    [ Font.color Ui.Theme.green ]
                    { url = "https://github.com/wolfadex/space-sim"
                    , label = text "GitHub page."
                    }
                ]
            ]
        )
