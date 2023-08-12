port module Shared exposing
    ( Effect(..)
    , Enabled(..)
    , Flags
    , GenerationConfig
    , PlayType(..)
    , Settings
    , SettingsFormDelta
    , SettingsFormState
    , SharedModel
    , SharedMsg(..)
    , defaultSettings
    , generationConfigCodec
    , init
    , update
    , viewSettings
    )

import Control
import Data.Name exposing (Name)
import Html exposing (Html)
import Input.Slider.Float
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import List.Nonempty exposing (Nonempty)
import Percent exposing (Percent)
import Random exposing (Seed)
import Serialize exposing (Codec)
import Ui
import Ui.Theme



-- import WebAudio


init : Flags -> SharedModel
init flags =
    let
        settings : Settings
        settings =
            case decodeSettings flags.settings of
                Err _ ->
                    defaultSettings

                Ok settings_ ->
                    settings_
    in
    { seed = Random.initialSeed flags.initialSeed
    , settings = settings
    , settingsModel =
        settingsForm.initWith settings
            |> Tuple.first
    }


type alias Flags =
    { initialSeed : Int
    , settings : Value
    }


type alias SharedModel =
    { settings : Settings
    , settingsModel : Control.State SettingsFormState
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
    = SettingsFormSentMsg (Control.Delta SettingsFormDelta)
    | SettingsFormSubmitted



---- SETTINGS ----


update : SharedMsg -> SharedModel -> ( SharedModel, Cmd SharedMsg )
update msg ({ settings } as model) =
    case msg of
        SettingsFormSentMsg msg_ ->
            let
                ( settingsModel, cmd ) =
                    settingsForm.update msg_ model.settingsModel
            in
            ( { model | settingsModel = settingsModel }
            , cmd
            )

        SettingsFormSubmitted ->
            let
                ( settingsModel, result ) =
                    settingsForm.submit model.settingsModel
            in
            ( { model
                | settingsModel = settingsModel
                , settings =
                    case result of
                        Ok newSettings ->
                            newSettings

                        Err _ ->
                            model.settings
              }
            , case result of
                Ok newSettings ->
                    saveSettings (encodeSettings newSettings)

                Err _ ->
                    Cmd.none
            )


port saveSettings : Value -> Cmd msg


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


viewSettings : SharedModel -> Html SharedMsg
viewSettings model =
    Ui.el
        [ Ui.justifySelf.end
        , Ui.width.shrink
        , Ui.height.shrink
        , Ui.padding.rem1
        ]
        (Ui.column
            [ Ui.backgroundColor Ui.Theme.nearlyWhite
            , Ui.padding.rem1
            , Ui.borderStyle.solid
            , Ui.borderColor Ui.Theme.darkGray
            , Ui.borderWidth.px3
            , Ui.borderRadius.remHalf
            , Ui.gap.remHalf
            ]
            [ Ui.el
                [ Ui.fontSize.rem2
                , Ui.borderWidth.bottom.px1
                , Ui.borderStyle.solid
                ]
                (Ui.text "Settings")
            , settingsForm.view model.settingsModel
            , Ui.paragraph
                [ Ui.fontSize.rem1
                , Ui.fontColor Ui.Theme.nearlyWhite
                , Ui.backgroundColor Ui.Theme.darkGray
                , Ui.padding.remQuarter
                , Ui.borderRadius.remQuarter
                ]
                [ Ui.text "For issues or to follow development, checkout the "
                , Ui.link.external
                    [ Ui.fontColor Ui.Theme.green ]
                    { url = "https://github.com/wolfadex/space-sim"
                    , label = Ui.text "GitHub page."
                    }
                ]
            ]
        )


type alias SettingsFormState =
    ( Control.State String
    , ( Control.State ( Control.State Bool, Control.End )
      , ( Control.State ( Control.State Bool, Control.End )
        , ( Control.State
                ( Control.State Input.Slider.Float.Model, Control.End )
          , ( Control.State ( Control.State Bool, Control.End )
            , Control.End
            )
          )
        )
      )
    )


type alias SettingsFormDelta =
    ( Control.Delta String
    , ( Control.Delta ( Control.Delta Bool, Control.End )
      , ( Control.Delta ( Control.Delta Bool, Control.End )
        , ( Control.Delta
                ( Control.Delta Input.Slider.Float.Msg, Control.End )
          , ( Control.Delta ( Control.Delta Bool, Control.End )
            , Control.End
            )
          )
        )
      )
    )


settingsForm : Control.Form SettingsFormState SettingsFormDelta Settings SharedMsg
settingsForm =
    Control.form
        { onUpdate = SettingsFormSentMsg
        , onSubmit = SettingsFormSubmitted
        , control =
            Control.record
                (\version realisticLighting planetsOrbit planetRotationSpeed showPlanetsOrbit ->
                    { version = version
                    , realisticLighting = realisticLighting
                    , planetsOrbit = planetsOrbit
                    , planetRotationSpeed = planetRotationSpeed
                    , showPlanetsOrbit = showPlanetsOrbit
                    }
                )
                |> Control.hiddenField .version Control.int
                |> Control.field .realisticLighting
                    (Control.bool
                        |> Control.label "Realistic Lighting"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.field .planetsOrbit
                    (Control.bool
                        |> Control.label "Planets Orbit"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.field .planetRotationSpeed
                    (Input.Slider.Float.new { min = 0.0, max = 1.0 }
                        |> Input.Slider.Float.withStep 0.01
                        |> Input.Slider.Float.toControl
                        |> Control.label "Planet Rotation Speed"
                        |> Control.map
                            { convert = Percent.fromFloat
                            , revert = Percent.toFloat
                            }
                    )
                |> Control.field .showPlanetsOrbit
                    (Control.bool
                        |> Control.label "Show Planets Orbit"
                        |> Control.map
                            { convert =
                                \b ->
                                    if b then
                                        Enabled

                                    else
                                        Disabled
                            , revert = \e -> e == Enabled
                            }
                    )
                |> Control.endRecord
        }
