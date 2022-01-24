module Shared exposing
    ( Effect(..)
    , Enabled(..)
    , Flags
    , Settings
    , SettingsMessage(..)
    , SharedModel
    , defaultSettings
    , encodeSettings
    , init
    , updateSettings
    , viewSettings
    )

import Data.Names exposing (CivilizationName)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Random exposing (Seed)
import Ui.Theme


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


type Effect
    = CreateGame { name : CivilizationName, homePlanetName : String, minSolarSystemsToGenerate : Int, maxSolarSystemsToGenerate : Int }
    | DeleteGame
    | UpdateSeed Seed
    | GotSharedSettingsChange SettingsMessage


type SettingsMessage
    = GotLightingChange Bool
    | GotPlanetOrbitChange Bool
    | GotShowPlanetOrbitChange Bool



---- SETTINGS ----


updateSettings : SettingsMessage -> Settings -> Settings
updateSettings msg settings =
    case msg of
        GotLightingChange enabledBool ->
            { settings
                | realisticLighting =
                    if enabledBool then
                        Enabled

                    else
                        Disabled
            }

        GotPlanetOrbitChange enabledBool ->
            { settings
                | planetsOrbit =
                    if enabledBool then
                        Enabled

                    else
                        Disabled
            }

        GotShowPlanetOrbitChange enabledBool ->
            { settings
                | showPlanetsOrbit =
                    if enabledBool then
                        Enabled

                    else
                        Disabled
            }


type alias Settings =
    { version : Int
    , realisticLighting : Enabled
    , planetsOrbit : Enabled
    , showPlanetsOrbit : Enabled
    }


defaultSettings : Settings
defaultSettings =
    { version = 0
    , realisticLighting = Enabled
    , planetsOrbit = Enabled
    , showPlanetsOrbit = Enabled
    }


encodeSettings : Settings -> Value
encodeSettings settings =
    Json.Encode.object
        [ ( "version", Json.Encode.int settings.version )
        , ( "realisticLighting", encodeEnabled settings.realisticLighting )
        , ( "planetsOrbit", encodeEnabled settings.planetsOrbit )
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
    Json.Decode.map3
        (\realisticLighting planetsOrbit showPlanetsOrbit ->
            { version = version
            , realisticLighting = realisticLighting
            , planetsOrbit = planetsOrbit
            , showPlanetsOrbit = showPlanetsOrbit
            }
        )
        (Json.Decode.field "realisticLighting" decodeEnabled)
        (Json.Decode.field "planetsOrbit" decodeEnabled)
        (Json.Decode.field "showPlanetsOrbit" decodeEnabled)


type Enabled
    = Enabled
    | Disabled


encodeEnabled : Enabled -> Value
encodeEnabled enabled =
    Json.Encode.bool <|
        case enabled of
            Enabled ->
                True

            Disabled ->
                False


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


viewSettings : Settings -> Element SettingsMessage
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
            ]
        )
