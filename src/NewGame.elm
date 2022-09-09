module NewGame exposing
    ( InnerPage(..)
    , Model
    , Msg
    , baseModel
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Data.Names exposing (CivilizationName)
import Data.Star
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Galaxy3d
import Game.Components exposing (CelestialBodyForm(..), LightYear, Orbit, Visible(..), Water)
import Length exposing (Meters)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Quantity
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
import Ui.Slider
import Ui.Text
import Ui.Theme
import Validator exposing (Validator)
import View exposing (View)



---- INIT ----


init : ( Model, SubCmd Msg Effect )
init =
    let
        ( _, m0 ) =
            Tuple.mapSecond (\m -> { m | civilizations = Set.singleton 0 }) (Logic.Entity.with ( Game.Components.civilizationPopulationSpec, Dict.singleton 1 Population.million ) (Logic.Entity.create 0 baseModel))

        starTemp : Temperature
        starTemp =
            Temperature.kelvins 3700

        ( _, m1 ) =
            Tuple.mapSecond (\m -> { m | stars = Set.singleton 1 }) (Logic.Entity.with ( Game.Components.parentSpec, 5 ) (Logic.Entity.with ( Data.Star.temperatureSpec, starTemp ) (Logic.Entity.create 1 m0)))

        ( _, m2 ) =
            Logic.Entity.with ( Game.Components.parentSpec, 5 ) (Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 ) (Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 75 ) (Logic.Entity.with ( Game.Components.orbitSpec, 6 ) (Logic.Entity.with ( Game.Components.planetTypeSpec, Gas ) (Logic.Entity.create 2 m1)))))

        ( _, m3 ) =
            Logic.Entity.with ( Game.Components.parentSpec, 5 ) (Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 ) (Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 15 ) (Logic.Entity.with ( Game.Components.orbitSpec, 8 ) (Logic.Entity.with ( Game.Components.planetTypeSpec, Gas ) (Logic.Entity.create 3 m2)))))

        ( _, m4 ) =
            Logic.Entity.with ( Game.Components.parentSpec, 5 ) (Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 ) (Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 80 ) (Logic.Entity.with ( Game.Components.orbitSpec, 4 ) (Logic.Entity.with ( Game.Components.planetTypeSpec, Rocky ) (Logic.Entity.create 4 m3)))))

        ( _, m5 ) =
            Tuple.mapSecond (\m -> { m | solarSystems = Set.singleton 5 }) (Logic.Entity.with ( Game.Components.positionSpec, Point3d.origin ) (Logic.Entity.create 5 m4))

        zoomDist : Float
        zoomDist =
            Length.inMeters (Quantity.multiplyBy (toFloat 8) Length.astronomicalUnit)
    in
    ( { m5
        | planets = Set.fromList [ 2, 3, 4 ]
        , zoom = zoomDist / 2
      }
    , Galaxy3d.getGalaxyViewport GotGalaxyViewport
    )


type alias Model =
    { page : InnerPage
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
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , civilizations : Set EntityID

    ---- game stuff
    , errors : List String
    , minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int

    -- participate only
    , civilizationNameSingular : String
    , civilizationNamePlural : String
    , hasUniquePluralName : Bool
    , civilizationNamePossessive : String
    , hasUniquePossessiveName : Bool
    , homePlanetName : String
    }


type InnerPage
    = MainMenu
    | Participate -- ParticipateModel
    | Observe -- ObserveModel


baseModel : Model
baseModel =
    { page = MainMenu
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
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , civilizations = Set.empty

    -- game stuff
    , civilizationNameSingular = ""
    , civilizationNamePlural = ""
    , hasUniquePluralName = True
    , civilizationNamePossessive = ""
    , hasUniquePossessiveName = True
    , homePlanetName = ""
    , errors = []
    , minSolarSystemsToGenerate = 40
    , maxSolarSystemsToGenerate = 80
    }



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]


type Msg
    = ViewParticipate
    | ViewObserve
    | Tick Float
    | WindowResized
    | GotGalaxyViewport (Result Browser.Dom.Error Viewport)
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg
    | ViewMain
      -- form stuff
    | SetNameSingular String
    | SetNamePlural String
    | ToggleNamePlural Bool
    | SetNamePossessive String
    | ToggleNamePossessive Bool
    | StartSimulation
    | SetHomePlanetName String
    | GotMinSolarSystemCount Int
    | GotMaxSolarSystemCount Int


update : SharedModel -> Msg -> Model -> ( Model, SubCmd Msg Effect )
update _ msg model =
    case msg of
        ViewParticipate ->
            ( { model | page = Participate }, SubCmd.none )

        ViewObserve ->
            ( { model | page = Observe }, SubCmd.none )

        -- ( ObserveMessage message, Observe mod ) ->
        --     updateObserve model message mod
        Tick deltaMs ->
            ( { model | elapsedTime = model.elapsedTime + deltaMs }, SubCmd.none )

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

        ViewMain ->
            ( { model | page = MainMenu }, SubCmd.none )

        -- form stuff
        SetNameSingular singular ->
            ( { model | civilizationNameSingular = singular }
            , SubCmd.none
            )

        SetNamePlural plural ->
            ( { model | civilizationNamePlural = plural }
            , SubCmd.none
            )

        ToggleNamePlural enabled ->
            ( { model | hasUniquePluralName = enabled }
            , SubCmd.none
            )

        SetNamePossessive possessive ->
            ( { model | civilizationNamePossessive = possessive }
            , SubCmd.none
            )

        ToggleNamePossessive enabled ->
            ( { model | hasUniquePossessiveName = enabled }
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

        StartSimulation ->
            case model.page of
                MainMenu ->
                    ( model, SubCmd.none )

                Observe ->
                    ( model
                    , SubCmd.effect
                        (Shared.CreateGame
                            Observation
                            { name = { singular = "", many = Nothing, possessive = Nothing }
                            , homePlanetName = ""
                            , minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                            , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                            }
                        )
                    )

                Participate ->
                    case Validator.run createGameValidator model of
                        Ok ( validName, validHomeName ) ->
                            ( model
                            , SubCmd.effect
                                (Shared.CreateGame
                                    Participation
                                    { name = validName
                                    , homePlanetName = validHomeName
                                    , minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                                    , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                                    }
                                )
                            )

                        Err errs ->
                            ( { model | errors = errs }, SubCmd.none )


createGameValidator : Validator Model String ( CivilizationName, String )
createGameValidator =
    Validator.map2 Tuple.pair
        civNameValidator
        homeNameValidator


homeNameValidator : Validator Model String String
homeNameValidator =
    Validator.required .homePlanetName String.isEmpty "Home planet name is required" (Validator.custom Ok) (Validator.succeed identity)


civNameValidator : Validator Model String CivilizationName
civNameValidator =
    Validator.required identity
        (\_ -> False)
        ""
        (Validator.custom possessiveNameValidator)
        (Validator.required identity
            (\_ -> False)
            ""
            (Validator.custom pluralNameValidator)
            (Validator.required .civilizationNameSingular
                String.isEmpty
                "Singular name is required"
                (Validator.custom Ok)
                (Validator.succeed
                    (\singular many possessive ->
                        { singular = singular
                        , many = many
                        , possessive = possessive
                        }
                    )
                )
            )
        )


pluralNameValidator : Model -> Result (List String) (Maybe String)
pluralNameValidator model =
    if model.hasUniquePluralName then
        if String.isEmpty model.civilizationNamePlural then
            Err [ "Plural name is required when enabled" ]

        else
            Ok (Just model.civilizationNamePlural)

    else
        Ok Nothing


possessiveNameValidator : Model -> Result (List String) (Maybe String)
possessiveNameValidator model =
    if model.hasUniquePossessiveName then
        if String.isEmpty model.civilizationNamePossessive then
            Err [ "Possessive name is required when enabled" ]

        else
            Ok (Just model.civilizationNamePossessive)

    else
        Ok Nothing



---- VIEW ----


view : SharedModel -> Model -> View Msg
view sharedModel model =
    let
        options : View Msg
        options =
            case model.page of
                MainMenu ->
                    viewMainMenu

                Participate ->
                    viewParticipate model

                Observe ->
                    viewObserve model
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
                (case model.page of
                    MainMenu ->
                        none

                    _ ->
                        el
                            [ padding 16 ]
                            (Ui.Button.default
                                { label = text "Main Menu"
                                , onPress = Just ViewMain
                                }
                            )
                )
            ]
            options.body
    }


contrastingBackground : Element msg -> Element msg
contrastingBackground =
    el
        [ Font.color Ui.Theme.nearlyWhite
        , Background.color (rgba 0 0 0 0.4)
        , padding 8
        , Border.rounded 8
        ]


viewMainMenu : View Msg
viewMainMenu =
    { title = "Hello Space!"
    , body =
        el
            [ padding 16
            , width fill
            , height fill
            ]
            (column
                [ centerX
                , centerY
                , spacing 64
                ]
                [ contrastingBackground (el [ centerX, Font.size 64 ] (text "Space Sim!"))
                , column
                    [ centerX, spacing 16 ]
                    [ el [ centerX ]
                        (Ui.Button.default
                            { onPress = Just ViewParticipate
                            , label = text "Participate"
                            }
                        )
                    , el [ centerX ]
                        (Ui.Button.default
                            { onPress = Just ViewObserve
                            , label = text "Observe"
                            }
                        )
                    , el [ centerX, Font.strike ]
                        (Ui.Button.default
                            { onPress = Nothing
                            , label = text "Load Simulation"
                            }
                        )
                    ]
                ]
            )
    }


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
                [ viewPlayerCivForm model
                , viewExample model
                ]
            ]
    }


viewPlayerCivForm : Model -> Element Msg
viewPlayerCivForm model =
    column
        [ spacing 16
        , width fill
        ]
        [ Ui.Text.default
            []
            { onChange = SetNameSingular
            , text = model.civilizationNameSingular
            , label = Input.labelLeft [ width fill ] (contrastingBackground (text "Civilization Name Singular:"))
            }
        , Ui.Text.default
            []
            { onChange = SetNamePlural
            , text = model.civilizationNamePlural
            , label = Input.labelLeft [ width fill ] (contrastingBackground (text "Civilization Name Plural:"))
            }
        , Ui.Button.default
            { label =
                text
                    (if model.hasUniquePluralName then
                        "Use '" ++ model.civilizationNameSingular ++ "' as the plural name"

                     else
                        "Use '" ++ model.civilizationNamePlural ++ "' as the plural name"
                    )
            , onPress = Just (ToggleNamePlural (not model.hasUniquePluralName))
            }
        , Ui.Text.default
            []
            { onChange = SetNamePossessive
            , text = model.civilizationNamePossessive
            , label = Input.labelLeft [ width fill ] (contrastingBackground (text "Civilization Name Possessive:"))
            }
        , Ui.Button.default
            { label =
                text
                    (if model.hasUniquePossessiveName then
                        "Use '" ++ model.civilizationNameSingular ++ "' as the possessive name"

                     else
                        "Use '" ++ model.civilizationNamePossessive ++ "' as the possessive name"
                    )
            , onPress = Just (ToggleNamePossessive (not model.hasUniquePossessiveName))
            }
        , Ui.Text.default
            []
            { onChange = SetHomePlanetName
            , text = model.homePlanetName
            , label = Input.labelLeft [ width fill ] (contrastingBackground (text "Home Planet Name:"))
            }
        , inputMaxSolarSystemCount model
        , inputMaxSolarSystemCount model
        , wrappedRow [ spacing 8 ] (List.map viewError model.errors)
        , startSimulationButton "Start Game"
        ]


startSimulationButton : String -> Element Msg
startSimulationButton label =
    el [ centerX ]
        (Ui.Button.default
            { label = text label
            , onPress = Just StartSimulation
            }
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
                    , displayGameValue "plural-name-example"
                        (if model.hasUniquePluralName then
                            model.civilizationNamePlural

                         else
                            model.civilizationNameSingular
                        )
                    , text " and the Federation, the "
                    , displayGameValue "singular-name-example" model.civilizationNameSingular
                    , text " people begin to question the morality of continuing the war. But the "
                    , displayGameValue "possessive-name-example"
                        (if model.hasUniquePossessiveName then
                            model.civilizationNamePossessive

                         else
                            model.civilizationNameSingular
                        )
                    , text " home planet, "
                    , displayGameValue "home-planet-name-example" model.homePlanetName
                    , text ", hangs in the balance."
                    ]
                ]
            )
        )


displayGameValue : String -> String -> Element msg
displayGameValue id value =
    el
        [ Font.color (rgb 1 0 1)
        , Element.Extra.id id
        ]
        (text value)


viewObserve : Model -> View Msg
viewObserve model =
    { title = "Hello Space! - Observe"
    , body =
        el
            [ padding 16
            , width fill
            , height fill
            ]
            (column
                [ centerX
                , centerY
                , spacing 64
                ]
                [ contrastingBackground (el [ centerX, Font.size 64, Font.underline ] (text "Observe the Simulation"))
                , wrappedRow
                    [ centerX
                    , centerY
                    , spacing 16
                    , padding 16
                    , width shrink
                    ]
                    [ viewObserveForm model
                    ]
                ]
            )
    }


viewObserveForm : Model -> Element Msg
viewObserveForm model =
    column
        [ spacing 16
        , width fill
        ]
        [ inputMinSolarSystemCount model
        , inputMaxSolarSystemCount model
        , startSimulationButton "Begin Simulation"
        ]


inputMaxSolarSystemCount : Model -> Element Msg
inputMaxSolarSystemCount model =
    Ui.Slider.int
        { onChange = GotMaxSolarSystemCount
        , label =
            Input.labelAbove []
                (paragraph []
                    [ contrastingBackground (text "Max Solar System Count: ")
                    , displayGameValue "max-solar-system-count" (String.fromInt model.maxSolarSystemsToGenerate)
                    ]
                )
        , min = 10
        , max = 800
        , value = model.maxSolarSystemsToGenerate
        , step = Just 10
        }


inputMinSolarSystemCount : Model -> Element Msg
inputMinSolarSystemCount model =
    Ui.Slider.int
        { onChange = GotMinSolarSystemCount
        , label =
            Input.labelAbove []
                (paragraph []
                    [ contrastingBackground (text "Min Solar System Count: ")
                    , displayGameValue "min-solar-system-count" (String.fromInt model.minSolarSystemsToGenerate)
                    ]
                )
        , min = 10
        , max = 800
        , value = model.minSolarSystemsToGenerate
        , step = Just 10
        }
