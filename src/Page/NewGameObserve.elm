module Page.NewGameObserve exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Control exposing (Control)
import Data.EarthYear
import Data.Name
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
import Html.Attributes
import Html.Events
import Input.MinMaxSlider
import Length exposing (Meters)
import List.Nonempty exposing (Nonempty)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Numeral
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

    ---- game stuff
    , errors : List String
    , minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
    , solarSystemForm : Control.State FormState
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

    -- game stuff
    , errors = []
    , minSolarSystemsToGenerate = 40
    , maxSolarSystemsToGenerate = 80
    , minPlanetsPerSolarSystemToGenerate = 1
    , maxPlanetsPerSolarSystemToGenerate = 12
    , starCounts =
        List.Nonempty.appendList
            [ ( 0.33, 2 ), ( 0.08, 3 ), ( 0.01, 4 ), ( 0.01, 5 ), ( 0.01, 6 ), ( 0.01, 7 ) ]
            (List.Nonempty.singleton ( 0.56, 1 ))
    , solarSystemForm = galaxyForm.init |> Tuple.first
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
    | GotMinSolarSystemCount Int
    | GotMaxSolarSystemCount Int
    | GotMinPlanetCount Int
    | GotMaxPlanetCount Int
    | GotStarCountChange Int Float
    | SolarSystemFormSentMsg (Control.Delta FormDelta)
    | SolarSystemFormSubmitted


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
        GotMinSolarSystemCount minCount ->
            ( { model
                | minSolarSystemsToGenerate = minCount
                , maxSolarSystemsToGenerate = max minCount model.maxSolarSystemsToGenerate
              }
            , SubCmd.none
            )

        GotMaxSolarSystemCount maxCount ->
            ( { model
                | minSolarSystemsToGenerate = min model.minSolarSystemsToGenerate maxCount
                , maxSolarSystemsToGenerate = maxCount
              }
            , SubCmd.none
            )

        GotMinPlanetCount minCount ->
            ( { model
                | minPlanetsPerSolarSystemToGenerate = minCount
                , maxPlanetsPerSolarSystemToGenerate = max minCount model.maxPlanetsPerSolarSystemToGenerate
              }
            , SubCmd.none
            )

        GotMaxPlanetCount maxCount ->
            ( { model
                | minPlanetsPerSolarSystemToGenerate = min model.minPlanetsPerSolarSystemToGenerate maxCount
                , maxPlanetsPerSolarSystemToGenerate = maxCount
              }
            , SubCmd.none
            )

        GotStarCountChange index newPercent ->
            let
                originalPercent : Float
                originalPercent =
                    Maybe.withDefault newPercent (Maybe.map Tuple.first (List.head (List.drop index (List.Nonempty.toList model.starCounts))))

                otherStarChange : Float
                otherStarChange =
                    (originalPercent - newPercent) / toFloat (List.Nonempty.length model.starCounts - 1)
            in
            ( { model
                | starCounts =
                    List.Nonempty.indexedMap
                        (\i ( percent, count ) ->
                            ( if i == index then
                                newPercent

                              else
                                min 1 (max 0 (percent + otherStarChange))
                            , count
                            )
                        )
                        model.starCounts
              }
            , SubCmd.none
            )

        SolarSystemFormSentMsg msg_ ->
            let
                ( solarSystemForm, cmd ) =
                    galaxyForm.update msg_ model.solarSystemForm
            in
            ( { model | solarSystemForm = solarSystemForm }
            , SubCmd.cmd cmd
            )

        SolarSystemFormSubmitted ->
            Debug.todo ""



---- VIEW ----


view : SharedModel -> Model -> View Msg
view sharedModel model =
    let
        options : View Msg
        options =
            viewObserve model
    in
    { title = options.title
    , body =
        Ui.el
            [ Ui.height.fill

            -- , behindContent
            --     (Galaxy3d.viewSolarSystem
            --         { onPressStar = Nothing
            --         , onPressPlanet = Nothing
            --         , onZoom = Nothing
            --         , onZoomPress = Nothing
            --         , onRotationPress = Nothing
            --         , focusedCivilization = Nothing
            --         , stars = model.stars
            --         , planets = model.planets
            --         }
            --         sharedModel.settings
            --         model
            --     )
            -- , inFront
            --     (el
            --         [ alignRight
            --         , alignTop
            --         , padding 16
            --         , inFront
            --             (case model.settingsVisible of
            --                 Hidden ->
            --                     none
            --                 Visible ->
            --                     map GotLocalSharedMessage (Shared.viewSettings sharedModel.settings)
            --             )
            --         ]
            --         (Ui.Button.default
            --             { label = text "⚙"
            --             , onPress =
            --                 Just
            --                     (case model.settingsVisible of
            --                         Visible ->
            --                             GotSettingsVisible Hidden
            --                         Hidden ->
            --                             GotSettingsVisible Visible
            --                     )
            --             }
            --         )
            --     )
            -- , inFront
            --     (el
            --         [ padding 16 ]
            --         (Ui.Link.internal
            --             { label = text "Main Menu"
            --             , route = Route.Home
            --             }
            --         )
            --     )
            ]
            options.body
    }


contrastingBackground : Html msg -> Html msg
contrastingBackground =
    Ui.el
        [ Ui.fontColor Ui.Theme.darkGray
        , Ui.backgroundColor Ui.Theme.nearlyWhiteTransparent
        , Ui.padding.remHalf
        , Ui.borderRadius.remHalf
        ]


displayGameValue : String -> String -> Html msg
displayGameValue id value =
    Ui.el
        [ -- Font.color (rgb 0.2 0.6 0.6)
          -- ,
          Html.Attributes.id id
        ]
        (Ui.text value)


viewObserve : Model -> View Msg
viewObserve model =
    { title = "Hello Space! - Observe"
    , body =
        Ui.el
            [ Ui.padding.rem1
            , Ui.height.fill
            ]
            (Ui.column
                [ -- centerX
                  -- , centerY
                  -- , spacing 64
                  Ui.gap.rem3
                ]
                [ contrastingBackground
                    (Ui.el
                        [-- centerX
                         -- , Font.size 64
                         -- , Font.underline
                        ]
                        (Ui.text "Observe the Simulation")
                    )
                , Ui.column
                    [ -- centerX
                      -- , centerY
                      -- ,
                      Ui.gap.rem1
                    , Ui.padding.rem1

                    -- , width shrink
                    ]
                    [ contrastingBackground (viewObserveForm model)
                    , Ui.el
                        [-- centerX
                        ]
                        (Ui.Link.internal []
                            { label = Ui.text "Begin Simulation"
                            , route =
                                Route.Playing
                                    { name = Data.Name.fromString ""
                                    , homePlanetName = ""
                                    , minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                                    , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                                    , minPlanetsPerSolarSystemToGenerate = model.minPlanetsPerSolarSystemToGenerate
                                    , maxPlanetsPerSolarSystemToGenerate = model.maxPlanetsPerSolarSystemToGenerate
                                    , starCounts = model.starCounts
                                    , playType = Shared.Observation
                                    }
                            }
                        )
                    ]
                ]
            )
    }


viewObserveForm : Model -> Html Msg
viewObserveForm model =
    Ui.el
        [-- Ui.height (px 600)
         -- , scrollbarY
        ]
        (galaxyForm.view model.solarSystemForm)


type alias FormState =
    ( Control.State Input.MinMaxSlider.Model
    , ( Control.State Input.MinMaxSlider.Model
      , ( Control.State (List.Nonempty.Nonempty ( Float, Int ))
        , Control.End
        )
      )
    )


type alias FormDelta =
    ( Control.Delta Input.MinMaxSlider.Msg
    , ( Control.Delta Input.MinMaxSlider.Msg
      , ( Control.Delta ( Int, Float ), Control.End )
      )
    )


type alias SolarSystemForm =
    { minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )
    }


galaxyForm : Control.Form FormState FormDelta SolarSystemForm Msg
galaxyForm =
    Control.form
        { onUpdate = SolarSystemFormSentMsg
        , onSubmit = SolarSystemFormSubmitted
        , control =
            Control.record
                (\solarSystems planets starCounts ->
                    { minSolarSystemsToGenerate = solarSystems.min
                    , maxSolarSystemsToGenerate = solarSystems.max
                    , minPlanetsPerSolarSystemToGenerate = planets.min
                    , maxPlanetsPerSolarSystemToGenerate = planets.max
                    , starCounts = starCounts
                    }
                )
                |> Control.field
                    (\{ minSolarSystemsToGenerate, maxSolarSystemsToGenerate } ->
                        { min = minSolarSystemsToGenerate, max = maxSolarSystemsToGenerate }
                    )
                    (Input.MinMaxSlider.new { min = 10, max = 800 }
                        |> Input.MinMaxSlider.withStep 10
                        |> Input.MinMaxSlider.toControl
                        |> Control.label "Solar Systems to Generate:"
                        |> Control.initWith { min = 40, max = 80 }
                    )
                |> Control.field
                    (\{ minPlanetsPerSolarSystemToGenerate, maxPlanetsPerSolarSystemToGenerate } ->
                        { min = minPlanetsPerSolarSystemToGenerate, max = maxPlanetsPerSolarSystemToGenerate }
                    )
                    (Input.MinMaxSlider.new { min = 0, max = 40 }
                        |> Input.MinMaxSlider.withStep 1
                        |> Input.MinMaxSlider.toControl
                        |> Control.label "Planets per Solar System:"
                        |> Control.initWith { min = 1, max = 12 }
                    )
                |> Control.field .starCounts starCountControl
                |> Control.endRecord
        }


starCountControl : Control (Nonempty ( Float, Int )) ( Int, Float ) (Nonempty ( Float, Int ))
starCountControl =
    Control.create
        { label = "Odds that a Solar System has:"
        , initEmpty =
            ( List.Nonempty.appendList
                [ ( 0.33, 2 ), ( 0.08, 3 ), ( 0.01, 4 ), ( 0.01, 5 ), ( 0.01, 6 ), ( 0.01, 7 ) ]
                (List.Nonempty.singleton ( 0.56, 1 ))
            , Cmd.none
            )
        , initWith = \starCounts -> ( starCounts, Cmd.none )
        , update = updateStarCountControl
        , view = viewStarCountControl
        , subscriptions = \_ -> Sub.none
        , parse = Ok
        }


updateStarCountControl : ( Int, Float ) -> Nonempty ( Float, Int ) -> ( Nonempty ( Float, Int ), Cmd ( Int, Float ) )
updateStarCountControl ( index, newPercent ) starCounts =
    let
        originalPercent : Float
        originalPercent =
            List.Nonempty.toList starCounts
                |> List.drop index
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault newPercent

        otherStarChange : Float
        otherStarChange =
            (originalPercent - newPercent) / toFloat (List.Nonempty.length starCounts - 1)
    in
    ( List.Nonempty.indexedMap
        (\i ( percent, count ) ->
            ( if i == index then
                newPercent

              else
                min 1 (max 0 (percent + otherStarChange))
            , count
            )
        )
        starCounts
    , Cmd.none
    )


viewStarCountControl :
    { state : Nonempty ( Float, Int )
    , class : String
    , id : String
    , name : String
    , label : String
    }
    -> List (Html ( Int, Float ))
viewStarCountControl { state, class, id, name, label } =
    [ Html.label
        [ Html.Attributes.for name ]
        [ Html.text label ]
    , Html.div
        [ Html.Attributes.class class
        , Html.Attributes.id id
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "padding-left" "1rem"
        ]
        (state
            |> List.Nonempty.toList
            |> List.concatMap
                (\( percent, count ) ->
                    [ Html.label
                        [ Html.Attributes.for (name ++ "stars-" ++ String.fromInt count) ]
                        [ Html.text (String.fromInt count ++ " Stars: " ++ Numeral.format "0.00[%]" percent) ]
                    , Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Attributes.name name
                        , Html.Attributes.min (String.fromFloat 0.0)
                        , Html.Attributes.max (String.fromFloat 1.0)
                        , Html.Attributes.step (String.fromFloat 0.001)
                        , Html.Attributes.value (String.fromFloat percent)
                        , Html.Events.onInput
                            (String.toFloat
                                >> Maybe.withDefault percent
                                >> Tuple.pair (count - 1)
                            )
                        ]
                        []
                    ]
                )
        )
    ]
