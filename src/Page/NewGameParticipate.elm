module Page.NewGameParticipate exposing
    ( CivilizationFormState
    , Model
    , Msg
    , Tab
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Control
import CubicSpline2d exposing (CubicSpline2d)
import Data.Civilization exposing (Sense)
import Data.EarthYear
import Data.Name exposing (Name)
import Data.Orbit exposing (Orbit)
import Data.Star
import Dict exposing (Dict)
import Galaxy3d
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , LightYear
        , SolarSystem(..)
        , Visible(..)
        , Water
        )
import Html exposing (Html)
import Html.Events
import Input.Spline
import Length exposing (Meters)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Page.Shared
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Quantity exposing (Unitless)
import Route exposing (PlayType(..))
import Scalable
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared
    exposing
        ( Effect(..)
        , SharedModel
        , SharedMsg
        )
import SubCmd exposing (SubCmd)
import Temperature exposing (Temperature)
import Ui
import Ui.Button
import Ui.Link
import Ui.Theme
import View exposing (View)



-- import WebAudio
-- import WebAudio.Property
---- INIT ----


init : ( Model, SubCmd Msg Effect )
init =
    let
        ( _, m0 ) =
            Logic.Entity.create 0 baseModel
                |> Logic.Entity.with ( Game.Components.civilizationPopulationSpec, Dict.singleton 1 Population.million )
                |> Tuple.mapSecond (\m -> { m | civilizations = Set.singleton 0 })

        starTemp : Temperature
        starTemp =
            Temperature.kelvins 3700

        ( _, m1 ) =
            Logic.Entity.create 1 m0
                |> Logic.Entity.with ( Data.Star.temperatureSpec, starTemp )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )
                |> Tuple.mapSecond (\m -> { m | stars = Set.singleton 1 })

        ( _, m2 ) =
            Logic.Entity.create 2 m1
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Gas )
                |> Logic.Entity.with
                    ( Game.Components.orbitSpec
                    , Data.Orbit.create
                        { distance = Length.astronomicalUnits 12
                        , period = Data.EarthYear.earthYears 250
                        }
                    )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 0.75 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m3 ) =
            Logic.Entity.create 3 m2
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Gas )
                |> Logic.Entity.with
                    ( Game.Components.orbitSpec
                    , Data.Orbit.create
                        { distance = Length.astronomicalUnits 6
                        , period = Data.EarthYear.earthYears 75
                        }
                    )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 0.15 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m4 ) =
            Logic.Entity.create 4 m3
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Rocky )
                |> Logic.Entity.with
                    ( Game.Components.orbitSpec
                    , Data.Orbit.create
                        { distance = Length.astronomicalUnits 3
                        , period = Data.EarthYear.earthYears 50.0
                        }
                    )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 0.8 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m5 ) =
            Logic.Entity.create 5 m4
                |> Logic.Entity.with ( Game.Components.positionSpec, Point3d.origin )
                |> Logic.Entity.with ( Game.Components.solarSystemSpec, SolarSystem )

        zoomDist : Float
        zoomDist =
            Length.astronomicalUnit
                |> Quantity.multiplyBy (toFloat 8)
                |> Length.inMeters
    in
    ( { m5
        | planets = Set.fromList [ 2, 3, 4 ]
        , zoom = zoomDist / 2
      }
    , Galaxy3d.getGalaxyViewport GotGalaxyViewport
    )



-- testSound : { a | frequency : Float } -> List WebAudio.Node
-- testSound model =
--     [ WebAudio.oscillator
--         [ WebAudio.Property.frequency 3
--         , WebAudio.Property.type_ "sawtooth"
--         ]
--         [ WebAudio.gain
--             [ WebAudio.Property.gain 40
--             ]
--             [ WebAudio.oscillator
--                 [ WebAudio.Property.frequency model.frequency -- 300
--                 ]
--                 [ WebAudio.audioDestination
--                 ]
--             ]
--         ]
--     ]


type alias Model =
    { frequency : Float
    , frequencyDirection : Float
    , settingsVisible : Visible
    , elapsedTime : Float
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

    ---- game stuff
    , tab : Tab

    -- solar systems
    , solarSystemForm : Control.State Page.Shared.GalaxyFormState

    -- civilizaiton
    , cooperationVsCompetition : Float
    , senses : AnySet Int Sense
    , descisionMakingStructure : Float
    , civilizationFormModel : Control.State CivilizationFormState
    }


type Tab
    = SolarSystemTab
    | CivilizationTab


baseModel : Model
baseModel =
    { frequency = 500
    , frequencyDirection = 1
    , settingsVisible = Hidden
    , elapsedTime = 1234345
    , galaxyViewSize = { width = 800, height = 600 }
    , zoom = -40
    , viewRotation = 0
    , civilizationPopulations = Logic.Component.empty
    , planetTypes = Logic.Component.empty
    , starTemperature = Logic.Component.empty
    , orbits = Logic.Component.empty
    , waterContent = Logic.Component.empty
    , planetSize = Logic.Component.empty
    , parents = Logic.Component.empty
    , galaxyPositions = Logic.Component.empty
    , solarSystems = Logic.Component.empty
    , planets = Set.empty
    , stars = Set.empty
    , civilizations = Set.empty
    , tab = SolarSystemTab

    ---- game stuff
    -- solar systems
    , solarSystemForm = galaxyForm.init |> Tuple.first

    -- civilization
    , cooperationVsCompetition = 0.5
    , senses = Set.Any.empty
    , descisionMakingStructure = 0.5
    , civilizationFormModel = civilizationForm.init |> Tuple.first
    }



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]


type Msg
    = Tick Float
    | WindowResized
    | GotGalaxyViewport (Result Browser.Dom.Error Viewport)
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg
      -- form stuff
    | TabChanged Tab
    | SolarSystemFormSentMsg (Control.Delta Page.Shared.GalaxyFormDelta)
    | SolarSystemFormSubmitted
    | CivilizationFormSentMsg (Control.Delta CivilizationFormDelta)
    | CivilizationFormSubmitted


update : SharedModel -> Msg -> Model -> ( Model, SubCmd Msg Effect )
update _ msg model =
    case msg of
        Tick deltaMs ->
            let
                newModel : Model
                newModel =
                    { model
                        | elapsedTime = model.elapsedTime + deltaMs
                        , frequency = model.frequency + deltaMs * model.frequencyDirection
                        , frequencyDirection =
                            if model.frequency >= 200 then
                                -1

                            else if model.frequency <= 10 then
                                1

                            else
                                model.frequencyDirection
                    }
            in
            ( newModel
              -- , SubCmd.effect (PlayAudio (testSound newModel))
            , SubCmd.none
            )

        WindowResized ->
            ( model, Galaxy3d.getGalaxyViewport GotGalaxyViewport )

        GotGalaxyViewport (Ok { viewport }) ->
            ( { model | galaxyViewSize = { width = viewport.width, height = viewport.height - 1 } }
            , SubCmd.none
            )

        GotGalaxyViewport (Err _) ->
            ( model, SubCmd.none )

        GotSettingsVisible visible ->
            ( { model | settingsVisible = visible }, SubCmd.none )

        GotLocalSharedMessage settingsChange ->
            ( model, SubCmd.effect (GotSharedMessage settingsChange) )

        -- form stuff
        TabChanged tab ->
            ( { model | tab = tab }, SubCmd.none )

        SolarSystemFormSentMsg msg_ ->
            let
                ( solarSystemForm, cmd ) =
                    galaxyForm.update msg_ model.solarSystemForm
            in
            ( { model | solarSystemForm = solarSystemForm }
            , SubCmd.cmd cmd
            )

        SolarSystemFormSubmitted ->
            let
                ( solarSystemForm, solarSystemResult ) =
                    galaxyForm.submit model.solarSystemForm

                ( civilizationFormModel, civilizationResult ) =
                    civilizationForm.submit model.civilizationFormModel
            in
            ( { model
                | solarSystemForm = solarSystemForm
                , civilizationFormModel = civilizationFormModel
              }
            , Result.map2
                (\galaxtOptions civilizationOptions ->
                    Route.Playing
                        { minSolarSystemsToGenerate = galaxtOptions.minSolarSystemsToGenerate
                        , maxSolarSystemsToGenerate = galaxtOptions.maxSolarSystemsToGenerate
                        , minPlanetsPerSolarSystemToGenerate = galaxtOptions.minPlanetsPerSolarSystemToGenerate
                        , maxPlanetsPerSolarSystemToGenerate = galaxtOptions.maxPlanetsPerSolarSystemToGenerate
                        , starCounts = galaxtOptions.starCounts
                        , playerStuff =
                            Just
                                { name = civilizationOptions.name
                                , homePlanetName = civilizationOptions.planetName
                                , reproductionMotivation =
                                    Scalable.new
                                        { spline = civilizationOptions.reproductionMotivation
                                        , initialInput = 1
                                        , min = 0
                                        , max = 1
                                        }
                                }
                        }
                        |> NavigateTo
                        |> SubCmd.effect
                )
                solarSystemResult
                civilizationResult
                |> Result.toMaybe
                |> Maybe.withDefault SubCmd.none
            )

        CivilizationFormSentMsg msg_ ->
            let
                ( civilizationFormModel, cmd ) =
                    civilizationForm.update msg_ model.civilizationFormModel
            in
            ( { model | civilizationFormModel = civilizationFormModel }
            , SubCmd.cmd cmd
            )

        CivilizationFormSubmitted ->
            Debug.todo ""



---- VIEW ----


view : SharedModel -> Model -> View Msg
view sharedModel model =
    { title = "Hello Space! - Participate"
    , body =
        Ui.stack
            [ Ui.height.fill
            ]
            [ Galaxy3d.viewSolarSystem
                { onPressStar = Nothing
                , onPressPlanet = Nothing
                , onZoom = Nothing
                , onZoomPress = Nothing
                , onRotationPress = Nothing
                , focusedCivilization = Nothing
                , stars = model.stars
                , planets = model.planets
                }
                sharedModel.settings
                model
            , viewParticipate model
            , Ui.column
                [ Ui.height.shrink
                , Ui.width.shrink
                , Ui.justifySelf.end
                ]
                [ Ui.Button.default
                    [ Ui.width.shrink
                    , Ui.height.shrink
                    , Ui.justifySelf.end
                    , Ui.transform
                        [ Ui.translate.left 10
                        , Ui.translate.down 10
                        ]
                    ]
                    { label = Ui.text "⚙"
                    , onPress =
                        (case model.settingsVisible of
                            Visible ->
                                Hidden

                            Hidden ->
                                Visible
                        )
                            |> GotSettingsVisible
                            |> Just
                    }
                , case model.settingsVisible of
                    Hidden ->
                        Ui.none

                    Visible ->
                        Shared.viewSettings sharedModel
                            |> Ui.map GotLocalSharedMessage
                            |> Ui.el
                                [ Ui.transform
                                    [ Ui.translate.down 16
                                    , Ui.translate.left 8
                                    ]
                                ]
                ]
            , Ui.el
                [ Ui.padding.rem1
                , Ui.width.shrink
                , Ui.height.shrink
                ]
                (Ui.Link.internal
                    []
                    { label = Ui.text "Main Menu"
                    , route = Route.Home
                    }
                )
            ]
    }


contrastingBackground : Html msg -> Html msg
contrastingBackground =
    Ui.el
        [ Ui.fontColor Ui.Theme.darkGray
        , Ui.backgroundColor Ui.Theme.nearlyWhiteTransparent
        , Ui.padding.remHalf
        , Ui.borderRadius.remHalf
        ]


viewParticipate : Model -> Html Msg
viewParticipate model =
    Ui.column
        [ Ui.gap.rem3
        , Ui.width.shrink
        , Ui.height.shrink
        , Ui.justifySelf.center
        , Ui.alignSelf.center
        ]
        [ Ui.text "Participate in the Simulation"
            |> Ui.el
                [ Ui.fontSize.rem3
                , Ui.fontUnderline
                ]
            |> contrastingBackground
            |> Ui.el
                [ Ui.justifySelf.center
                , Ui.width.shrink
                ]
        , Ui.column
            [ Ui.gap.rem1
            , Ui.width.shrink
            , Ui.justifySelf.center
            ]
            [ case model.tab of
                SolarSystemTab ->
                    galaxyForm.view model.solarSystemForm
                        |> contrastingBackground

                -- |> Ui.el [ Ui.width.shrink ]
                CivilizationTab ->
                    civilizationForm.view model.civilizationFormModel
                        |> contrastingBackground

            -- TODO
            -- , case Validator.run createGameValidator model of
            --     Ok _ ->
            --         Ui.el
            --             [-- height (px 36)
            --             ]
            --             Ui.none
            --     Err errors ->
            --         Ui.rowWrapped [ Ui.gap.remHalf ] (List.map viewError errors)
            , Ui.column [ Ui.gap.rem1 ] <|
                case model.tab of
                    SolarSystemTab ->
                        [ Ui.Button.default []
                            { label = Ui.text "Setup Civilization"
                            , onPress = Just (TabChanged CivilizationTab)
                            }
                        ]

                    CivilizationTab ->
                        [ Ui.Button.default []
                            { label = Ui.text "Change Solar System"
                            , onPress = Just (TabChanged SolarSystemTab)
                            }
                        , Ui.Button.primary []
                            { label = Ui.text "Start Game"
                            , onPress = Just SolarSystemFormSubmitted
                            }
                        ]
            ]
        ]


galaxyForm : Control.Form Page.Shared.GalaxyFormState Page.Shared.GalaxyFormDelta Page.Shared.GalaxyFormResult Msg
galaxyForm =
    Page.Shared.galaxyForm { toMsg = SolarSystemFormSentMsg, onSubmit = SolarSystemFormSubmitted }


type alias CivilizationFormState =
    ( Control.State ( Control.State String, Control.End )
    , ( Control.State String
      , ( Control.State (Input.Spline.Model MotivationSpline), Control.End )
      )
    )


type MotivationSpline
    = MotivationSpline Never


type alias CivilizationFormDelta =
    ( Control.Delta ( Control.Delta String, Control.End )
    , ( Control.Delta String
      , ( Control.Delta Input.Spline.Msg, Control.End )
      )
    )


type alias CivilizationFormResult =
    { name : Name
    , planetName : String
    , reproductionMotivation : CubicSpline2d Unitless MotivationSpline
    }


civilizationForm : Control.Form CivilizationFormState CivilizationFormDelta CivilizationFormResult Msg
civilizationForm =
    Control.form
        { control =
            Control.record
                (\name planetName reproductionMotivation ->
                    { name = name
                    , planetName = planetName
                    , reproductionMotivation = reproductionMotivation
                    }
                )
                |> Control.field .name
                    (Control.string
                        |> Control.label "Name"
                        |> Control.failIf String.isEmpty "Name cannot be empty"
                        |> Control.map
                            { convert = Data.Name.fromString
                            , revert = Data.Name.toString
                            }
                    )
                |> Control.field .planetName
                    (Control.string
                        |> Control.label "Home Planet Name"
                        |> Control.failIf String.isEmpty "Home Planet Name cannot be empty"
                    )
                |> Control.field .reproductionMotivation
                    (Input.Spline.new
                        { xMin = 0
                        , xMax = 1
                        , yMin = 0
                        , yMax = 1
                        }
                        |> Input.Spline.toControl
                        |> Control.label "Reproduction Motivation"
                    )
                |> Control.endRecord
        , onUpdate = CivilizationFormSentMsg
        , view =
            \fields ->
                Html.form [ Html.Events.onSubmit CivilizationFormSubmitted ]
                    fields
        }
