module Page.NewGameParticipate exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Data.EarthYear
import Data.Name exposing (Name)
import Data.Orbit exposing (Orbit)
import Data.Star
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Galaxy3d
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , LightYear
        , SolarSystem(..)
        , Visible(..)
        , Water
        )
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
        , PlayType(..)
        , SharedModel
        , SharedMsg
        )
import SubCmd exposing (SubCmd)
import Temperature exposing (Temperature)
import Ui.Button
import Ui.Link
import Ui.Slider
import Ui.Text
import Ui.Theme
import Validator exposing (Validator)
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
    , minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    , minPlanetsPerSolarSystemToGenerate : Int
    , maxPlanetsPerSolarSystemToGenerate : Int
    , starCounts : Nonempty ( Float, Int )

    -- participate only
    , civilizationNameSingular : String
    , homePlanetName : String
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
    , civilizationNameSingular = ""
    , homePlanetName = ""
    , minSolarSystemsToGenerate = 40
    , maxSolarSystemsToGenerate = 80
    , minPlanetsPerSolarSystemToGenerate = 1
    , maxPlanetsPerSolarSystemToGenerate = 12
    , starCounts =
        List.Nonempty.appendList
            [ ( 0.33, 2 ), ( 0.08, 3 ), ( 0.01, 4 ), ( 0.01, 5 ), ( 0.01, 6 ), ( 0.01, 7 ) ]
            (List.Nonempty.singleton ( 0.56, 1 ))
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
    | SetNameSingular String
    | SetHomePlanetName String
    | GotMinSolarSystemCount Int
    | GotMaxSolarSystemCount Int
    | GotMinPlanetCount Int
    | GotMaxPlanetCount Int
    | GotStarCountChange Int Float


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
        SetNameSingular singular ->
            ( { model | civilizationNameSingular = singular }
            , SubCmd.none
            )

        SetHomePlanetName name ->
            ( { model | homePlanetName = name }
            , SubCmd.none
            )

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


createGameValidator : Validator Model String ( Name, String )
createGameValidator =
    Validator.map2 Tuple.pair
        civNameValidator
        homeNameValidator


homeNameValidator : Validator Model String String
homeNameValidator =
    Validator.required .homePlanetName String.isEmpty "Home planet name is required" (Validator.custom Ok) (Validator.succeed identity)


civNameValidator : Validator Model String Name
civNameValidator =
    Validator.required .civilizationNameSingular
        String.isEmpty
        "Name is required"
        (Validator.custom Ok)
        (Validator.succeed Data.Name.fromString)



---- VIEW ----


view : SharedModel -> Model -> View Msg
view sharedModel model =
    let
        options : View Msg
        options =
            viewParticipate model
    in
    { title = options.title
    , body =
        el
            [ width fill
            , height fill
            , behindContent
                (Galaxy3d.viewSolarSystem
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
                )
            , inFront
                (el
                    [ alignRight
                    , alignTop
                    , padding 16
                    , inFront
                        (case model.settingsVisible of
                            Hidden ->
                                none

                            Visible ->
                                map GotLocalSharedMessage (Shared.viewSettings sharedModel.settings)
                        )
                    ]
                    (Ui.Button.default
                        { label = text "⚙"
                        , onPress =
                            Just
                                (case model.settingsVisible of
                                    Visible ->
                                        GotSettingsVisible Hidden

                                    Hidden ->
                                        GotSettingsVisible Visible
                                )
                        }
                    )
                )
            , inFront
                (el
                    [ padding 16 ]
                    (Ui.Link.internal
                        { label = text "Main Menu"
                        , route = Route.Home
                        }
                    )
                )
            ]
            options.body
    }


contrastingBackground : Element msg -> Element msg
contrastingBackground =
    el
        [ Font.color Ui.Theme.darkGray
        , Background.color Ui.Theme.nearlyWhiteTransparent
        , padding 8
        , Border.rounded 8
        ]


viewParticipate : Model -> View Msg
viewParticipate model =
    { title = "Hello Space! - Participate"
    , body =
        column
            [ centerX
            , centerY
            , spacing 64
            ]
            [ el [ centerX, Font.size 64, Font.underline ] (contrastingBackground (text "Participate in the Simulation"))
            , wrappedRow
                [ centerX
                , centerY
                , spacing 16
                , padding 16
                , width shrink
                ]
                [ column
                    [ centerY
                    , width fill
                    , spacing 16
                    , padding 16
                    ]
                    [ contrastingBackground (viewPlayerCivForm model)
                    , case Validator.run createGameValidator model of
                        Ok _ ->
                            none

                        Err errors ->
                            wrappedRow [ spacing 8 ] (List.map viewError errors)
                    , el [ centerX ]
                        (case Validator.run createGameValidator model of
                            Ok ( validName, validHomeName ) ->
                                Ui.Link.internal
                                    { label = text "Start Game"
                                    , route =
                                        Route.Playing
                                            { name = validName
                                            , homePlanetName = validHomeName
                                            , minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                                            , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                                            , minPlanetsPerSolarSystemToGenerate = model.minPlanetsPerSolarSystemToGenerate
                                            , maxPlanetsPerSolarSystemToGenerate = model.maxPlanetsPerSolarSystemToGenerate
                                            , starCounts = model.starCounts
                                            , playType = Participation
                                            }
                                    }

                            Err _ ->
                                Ui.Button.default
                                    { label = text "Start Game"
                                    , onPress = Nothing
                                    }
                        )
                    ]
                , viewExample model
                ]
            ]
    }


viewPlayerCivForm : Model -> Element Msg
viewPlayerCivForm model =
    column
        [ spacing 16
        , width fill
        , height (px 600)
        , scrollbarY
        ]
        (List.intersperse formSpacer
            [ inputGroup "Civilization"
                [ Ui.Text.default
                    []
                    { onChange = SetNameSingular
                    , text = model.civilizationNameSingular
                    , label = Input.labelLeft [ width fill ] (text "Name Singular:")
                    }
                , Ui.Text.default
                    []
                    { onChange = SetHomePlanetName
                    , text = model.homePlanetName
                    , label = Input.labelLeft [ width fill ] (text "Home Planet Name:")
                    }
                ]
            , inputSolarSystems model
            , inputPlanets model
            , inputStarCounts model
            ]
        )


viewError : String -> Element msg
viewError error =
    el
        [ Background.color Ui.Theme.nearlyWhite
        , Font.color Ui.Theme.error
        , paddingXY 16 8
        , Border.rounded 32
        ]
        (text error)


viewExample : Model -> Element Msg
viewExample model =
    el [ alignTop ]
        (contrastingBackground
            (column
                [ spacing 8
                , width (maximum 600 (minimum 400 fill))
                ]
                [ text "Example:"
                , paragraph
                    []
                    [ text
                        "As the battle rages on between the "
                    , el [ Font.underline ]
                        (displayGameValue "plural-name-example"
                            (showBlank
                                (Data.Name.toString (Data.Name.plurualize (Data.Name.fromString model.civilizationNameSingular)))
                            )
                        )
                    , text " and the Federation, the "
                    , el [ Font.underline ] (displayGameValue "singular-name-example" (showBlank model.civilizationNameSingular))
                    , text " people begin to question the morality of continuing the war. But the "
                    , el [ Font.underline ]
                        (displayGameValue "possessive-name-example"
                            (showBlank
                                (Data.Name.toString (Data.Name.possessive (Data.Name.fromString model.civilizationNameSingular)))
                            )
                        )
                    , text " home planet, "
                    , el [ Font.underline ] (displayGameValue "home-planet-name-example" (showBlank model.homePlanetName))
                    , text ", hangs in the balance."
                    ]
                ]
            )
        )


showBlank : String -> String
showBlank str =
    if String.isEmpty str then
        "____"

    else
        str


displayGameValue : String -> String -> Element msg
displayGameValue id value =
    el
        [ Font.color (rgb 0.2 0.6 0.6)
        , Element.Extra.id id
        ]
        (text value)


formSpacer : Element msg
formSpacer =
    el
        [ Border.width 1
        , Border.color (rgb 0.6 0.6 0.4)
        , width fill
        ]
        none


inputGroup : String -> List (Element Msg) -> Element Msg
inputGroup label inputs =
    column
        [ spacing 8
        , width fill
        , padding 16
        ]
        [ text (label ++ ":")
        , column
            [ spacing 8
            , paddingEach
                { left = 16
                , top = 0
                , right = 0
                , bottom = 0
                }
            , width fill
            ]
            inputs
        ]


inputSolarSystems : Model -> Element Msg
inputSolarSystems model =
    inputGroup "Solar System to Generate"
        [ Ui.Slider.int []
            { onChange = GotMinSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Min: "
                        , displayGameValue "min-solar-system-count" (String.fromInt model.minSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 800
            , value = model.minSolarSystemsToGenerate
            , step = Just 10
            }
        , Ui.Slider.int []
            { onChange = GotMaxSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Max : "
                        , displayGameValue "max-solar-system-count" (String.fromInt model.maxSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 800
            , value = model.maxSolarSystemsToGenerate
            , step = Just 10
            }
        ]


inputPlanets : Model -> Element Msg
inputPlanets model =
    inputGroup "Planets per Solar System"
        [ Ui.Slider.int []
            { onChange = GotMinPlanetCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Min: "
                        , displayGameValue "min-planet-count" (String.fromInt model.minPlanetsPerSolarSystemToGenerate)
                        ]
                    )
            , min = 0
            , max = 40
            , value = model.minPlanetsPerSolarSystemToGenerate
            , step = Just 1
            }
        , Ui.Slider.int []
            { onChange = GotMaxPlanetCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Max: "
                        , displayGameValue "max-planet-count" (String.fromInt model.maxPlanetsPerSolarSystemToGenerate)
                        ]
                    )
            , min = 1
            , max = 40
            , value = model.maxPlanetsPerSolarSystemToGenerate
            , step = Just 1
            }
        ]


inputStarCounts : Model -> Element Msg
inputStarCounts model =
    inputGroup "Odds that a Solar System has"
        (List.indexedMap
            (\index ( percent, count ) ->
                Ui.Slider.float []
                    { onChange = GotStarCountChange index
                    , label =
                        Input.labelAbove []
                            (paragraph []
                                [ text (String.fromInt count ++ " Stars: ")
                                , displayGameValue (String.fromInt count ++ "-star-count") (Numeral.format "0.00[%]" percent)
                                ]
                            )
                    , min = 0
                    , max = 1
                    , value = percent
                    , step = Just 0.0005
                    }
            )
            (List.Nonempty.toList model.starCounts)
        )
