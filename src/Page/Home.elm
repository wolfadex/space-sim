module Page.Home exposing
    ( Model
    , Msg
    , baseModel
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Data.EarthYear
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
import Length exposing (Meters)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Quantity
import Route
import Set exposing (Set)
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
    }


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



---- VIEW ----


view : SharedModel -> Model -> View Msg
view sharedModel model =
    let
        options : View Msg
        options =
            viewMainMenu
    in
    { title = options.title
    , body =
        Ui.stack
            [ Ui.height.fill ]
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
            , options.body
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
                        Ui.map GotLocalSharedMessage (Shared.viewSettings sharedModel)
                ]
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


viewMainMenu : View Msg
viewMainMenu =
    { title = "Hello Space!"
    , body =
        Ui.el
            [ Ui.padding.rem1
            , Ui.height.fill
            ]
            (Ui.column
                [ Ui.justifySelf.center
                , Ui.alignSelf.center
                , Ui.justifySelf.center
                , Ui.alignSelf.center
                , Ui.gap.rem3
                , Ui.width.shrink
                ]
                [ contrastingBackground
                    (Ui.el
                        [ Ui.justifySelf.center
                        , Ui.fontSize.rem3
                        ]
                        (Ui.text "Space Sim!")
                    )
                , Ui.column
                    [ Ui.justifySelf.center
                    , Ui.gap.rem1
                    ]
                    [ Ui.el [ Ui.justifySelf.center ]
                        (Ui.Link.internal [ Ui.width.shrink, Ui.justifySelf.center ]
                            { route = Route.NewGameParticipate
                            , label = Ui.text "Participate"
                            }
                        )
                    , Ui.el [ Ui.justifySelf.center ]
                        (Ui.Link.internal [ Ui.width.shrink, Ui.justifySelf.center ]
                            { route = Route.NewGameObserve
                            , label = Ui.text "Observe"
                            }
                        )
                    , Ui.el
                        [ Ui.justifySelf.center

                        -- , Font.strike
                        ]
                        (Ui.Button.default [ Ui.width.shrink, Ui.justifySelf.center ]
                            { onPress = Nothing
                            , label = Ui.text "Load Simulation"
                            }
                        )
                    ]
                ]
            )
    }
