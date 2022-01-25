module NewGame exposing
    ( InnerPage(..)
    , Model
    , Msg
    , ObserveModel
    , ParticipateModel
    , baseModel
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (Viewport)
import Browser.Events
import Data.Names exposing (CivilizationName)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Galaxy3d
import Game.Components exposing (CelestialBodyForm(..), LightYear, Orbit, StarSize(..), Visible(..), Water)
import Length exposing (Meters)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Set exposing (Set)
import Shared
    exposing
        ( Effect(..)
        , PlayType(..)
        , SharedModel
        , SharedMsg
        )
import SubCmd exposing (SubCmd)
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
            Logic.Entity.create 0 baseModel
                |> Logic.Entity.with ( Game.Components.civilizationPopulationSpec, Dict.singleton 1 Population.million )
                |> Tuple.mapSecond (\m -> { m | civilizations = Set.singleton 0 })

        ( _, m1 ) =
            Logic.Entity.create 1 m0
                |> Logic.Entity.with ( Game.Components.starFormSpec, Yellow )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )
                |> Tuple.mapSecond (\m -> { m | stars = Set.singleton 1 })

        ( _, m2 ) =
            Logic.Entity.create 2 m1
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Gas )
                |> Logic.Entity.with ( Game.Components.orbitSpec, 6 )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 75 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m3 ) =
            Logic.Entity.create 3 m2
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Gas )
                |> Logic.Entity.with ( Game.Components.orbitSpec, 8 )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 15 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m4 ) =
            Logic.Entity.create 4 m3
                |> Logic.Entity.with ( Game.Components.planetTypeSpec, Rocky )
                |> Logic.Entity.with ( Game.Components.orbitSpec, 4 )
                |> Logic.Entity.with ( Game.Components.waterSpec, Percent.fromFloat 80 )
                |> Logic.Entity.with ( Game.Components.planetSizeSpec, 40000 )
                |> Logic.Entity.with ( Game.Components.parentSpec, 5 )

        ( _, m5 ) =
            Logic.Entity.create 5 m4
                |> Logic.Entity.with ( Game.Components.positionSpec, Point3d.origin )
                |> Tuple.mapSecond (\m -> { m | solarSystems = Set.singleton 5 })
    in
    ( { m5 | planets = Set.fromList [ 2, 3, 4 ] }
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
    , starForms : Logic.Component.Set StarSize
    , orbits : Logic.Component.Set Orbit
    , waterContent : Logic.Component.Set (Percent Water)
    , planetSize : Logic.Component.Set Float
    , parents : Logic.Component.Set EntityID
    , galaxyPositions : Logic.Component.Set (Point3d Meters LightYear)
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , civilizations : Set EntityID
    }


type InnerPage
    = MainMenu
    | Participate ParticipateModel
    | Observe ObserveModel


baseModel : Model
baseModel =
    { page = MainMenu
    , settingsVisible = Hidden
    , elapsedTime = 1234345
    , galaxyViewSize = { width = 800, height = 600 }
    , zoom = -40
    , viewRotation = 0
    , civilizationPopulations = Logic.Component.empty -- (Dict EntityID Population)
    , planetTypes = Logic.Component.empty -- CelestialBodyForm
    , starForms = Logic.Component.empty -- StarSize
    , orbits = Logic.Component.empty -- Orbit
    , waterContent = Logic.Component.empty -- (Percent Water)
    , planetSize = Logic.Component.empty -- Float
    , parents = Logic.Component.empty -- EntityID
    , galaxyPositions = Logic.Component.empty -- (Point3d Meters LightYear)
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , civilizations = Set.empty
    }


type alias ParticipateModel =
    { civilizationNameSingular : String
    , civilizationNamePlural : String
    , hasUniquePluralName : Bool
    , civilizationNamePossessive : String
    , hasUniquePossessiveName : Bool
    , homePlanetName : String
    , errors : List String
    , minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    }


baseParticipateModel : ParticipateModel
baseParticipateModel =
    { civilizationNameSingular = ""
    , civilizationNamePlural = ""
    , hasUniquePluralName = True
    , civilizationNamePossessive = ""
    , hasUniquePossessiveName = True
    , homePlanetName = ""
    , errors = []
    , minSolarSystemsToGenerate = 100
    , maxSolarSystemsToGenerate = 300
    }


type alias ObserveModel =
    { minSolarSystemsToGenerate : Int
    , maxSolarSystemsToGenerate : Int
    }


baseObserveModel : ObserveModel
baseObserveModel =
    { minSolarSystemsToGenerate = 100
    , maxSolarSystemsToGenerate = 300
    }



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]


type Msg
    = MainMenuMessage MainMenuMsg
    | ParticipateMessage ParticipateMsg
    | ObserveMessage ObserveMsg
    | Tick Float
    | WindowResized
    | GotGalaxyViewport (Result Browser.Dom.Error Viewport)
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg
    | ViewMain


type MainMenuMsg
    = ViewParticipate
    | ViewObserve


type ParticipateMsg
    = SetNameSingular String
    | SetNamePlural String
    | ToggleNamePlural Bool
    | SetNamePossessive String
    | ToggleNamePossessive Bool
    | StartGame
    | SetHomePlanetName String
    | GotMinSolarSystemCount Int
    | GotMaxSolarSystemCount Int


type ObserveMsg
    = OGotMinSolarSystemCount Int
    | OGotMaxSolarSystemCount Int
    | BeginSimulation


update : SharedModel -> Msg -> Model -> ( Model, SubCmd Msg Effect )
update _ msg model =
    case ( msg, model.page ) of
        ( MainMenuMessage message, MainMenu ) ->
            updateMainMenu model message

        ( ParticipateMessage message, Participate mod ) ->
            updateParticipate model message mod

        ( ObserveMessage message, Observe mod ) ->
            updateObserve model message mod

        ( Tick deltaMs, _ ) ->
            ( { model | elapsedTime = model.elapsedTime + deltaMs }, SubCmd.none )

        ( WindowResized, _ ) ->
            ( model, Galaxy3d.getGalaxyViewport GotGalaxyViewport )

        ( GotGalaxyViewport (Ok { viewport }), _ ) ->
            ( { model | galaxyViewSize = { width = viewport.width, height = viewport.height - 1 } }
            , SubCmd.none
            )

        ( GotGalaxyViewport (Err _), _ ) ->
            ( model, SubCmd.none )

        ( GotSettingsVisible visible, _ ) ->
            ( { model | settingsVisible = visible }, SubCmd.none )

        ( GotLocalSharedMessage settingsChange, _ ) ->
            ( model, SubCmd.effect (GotSharedMessage settingsChange) )

        ( ViewMain, _ ) ->
            ( { model | page = MainMenu }, SubCmd.none )

        _ ->
            ( model, SubCmd.none )


updateMainMenu : Model -> MainMenuMsg -> ( Model, SubCmd Msg Effect )
updateMainMenu mainModel msg =
    case msg of
        ViewParticipate ->
            ( { mainModel | page = Participate baseParticipateModel }, SubCmd.none )

        ViewObserve ->
            ( { mainModel | page = Observe baseObserveModel }, SubCmd.none )


updateParticipate : Model -> ParticipateMsg -> ParticipateModel -> ( Model, SubCmd Msg Effect )
updateParticipate mainModel msg model =
    case msg of
        SetNameSingular singular ->
            ( { mainModel | page = Participate { model | civilizationNameSingular = singular } }
            , SubCmd.none
            )

        SetNamePlural plural ->
            ( { mainModel | page = Participate { model | civilizationNamePlural = plural } }
            , SubCmd.none
            )

        ToggleNamePlural enabled ->
            ( { mainModel | page = Participate { model | hasUniquePluralName = enabled } }
            , SubCmd.none
            )

        SetNamePossessive possessive ->
            ( { mainModel | page = Participate { model | civilizationNamePossessive = possessive } }
            , SubCmd.none
            )

        ToggleNamePossessive enabled ->
            ( { mainModel | page = Participate { model | hasUniquePossessiveName = enabled } }
            , SubCmd.none
            )

        SetHomePlanetName name ->
            ( { mainModel | page = Participate { model | homePlanetName = name } }
            , SubCmd.none
            )

        GotMinSolarSystemCount minCount ->
            ( { mainModel
                | page =
                    Participate
                        { model
                            | minSolarSystemsToGenerate = minCount
                            , maxSolarSystemsToGenerate = max minCount model.maxSolarSystemsToGenerate
                        }
              }
            , SubCmd.none
            )

        GotMaxSolarSystemCount maxCount ->
            ( { mainModel
                | page =
                    Participate
                        { model
                            | minSolarSystemsToGenerate = min model.minSolarSystemsToGenerate maxCount
                            , maxSolarSystemsToGenerate = maxCount
                        }
              }
            , SubCmd.none
            )

        StartGame ->
            case Validator.run createGameValidator model of
                Ok ( validName, validHomeName ) ->
                    ( mainModel
                    , SubCmd.effect
                        (Shared.CreateGame
                            (Participation
                                { name = validName
                                , homePlanetName = validHomeName
                                , minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                                , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                                }
                            )
                        )
                    )

                Err errs ->
                    ( { mainModel | page = Participate { model | errors = errs } }, SubCmd.none )


updateObserve : Model -> ObserveMsg -> ObserveModel -> ( Model, SubCmd Msg Effect )
updateObserve mainModel msg model =
    case msg of
        OGotMinSolarSystemCount minCount ->
            ( { mainModel
                | page =
                    Observe
                        { model
                            | minSolarSystemsToGenerate = minCount
                            , maxSolarSystemsToGenerate = max minCount model.maxSolarSystemsToGenerate
                        }
              }
            , SubCmd.none
            )

        OGotMaxSolarSystemCount maxCount ->
            ( { mainModel
                | page =
                    Observe
                        { model
                            | minSolarSystemsToGenerate = min model.minSolarSystemsToGenerate maxCount
                            , maxSolarSystemsToGenerate = maxCount
                        }
              }
            , SubCmd.none
            )

        BeginSimulation ->
            ( mainModel
            , SubCmd.effect
                (Shared.CreateGame
                    (Observation
                        { minSolarSystemsToGenerate = model.minSolarSystemsToGenerate
                        , maxSolarSystemsToGenerate = model.maxSolarSystemsToGenerate
                        }
                    )
                )
            )


createGameValidator : Validator ParticipateModel String ( CivilizationName, String )
createGameValidator =
    Validator.map2 Tuple.pair
        civNameValidator
        homeNameValidator


homeNameValidator : Validator ParticipateModel String String
homeNameValidator =
    Validator.succeed identity
        |> Validator.required .homePlanetName String.isEmpty "Home planet name is required" (Validator.custom Ok)


civNameValidator : Validator ParticipateModel String CivilizationName
civNameValidator =
    Validator.succeed
        (\singular many possessive ->
            { singular = singular
            , many = many
            , possessive = possessive
            }
        )
        |> Validator.required .civilizationNameSingular String.isEmpty "Singular name is required" (Validator.custom Ok)
        |> Validator.required identity (\_ -> False) "" (Validator.custom pluralNameValidator)
        |> Validator.required identity (\_ -> False) "" (Validator.custom possessiveNameValidator)


pluralNameValidator : ParticipateModel -> Result (List String) (Maybe String)
pluralNameValidator model =
    if model.hasUniquePluralName then
        if String.isEmpty model.civilizationNamePlural then
            Err [ "Plural name is required when enabled" ]

        else
            Ok (Just model.civilizationNamePlural)

    else
        Ok Nothing


possessiveNameValidator : ParticipateModel -> Result (List String) (Maybe String)
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

                Participate m ->
                    viewParticipate m

                Observe m ->
                    viewObserve m
    in
    { title = options.title
    , body =
        el
            [ width fill
            , height fill
            , behindContent <|
                Galaxy3d.viewSolarSystem
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
            , inFront
                (el
                    [ alignRight
                    , alignTop
                    , padding 16
                    , inFront <|
                        case model.settingsVisible of
                            Hidden ->
                                none

                            Visible ->
                                map GotLocalSharedMessage (Shared.viewSettings sharedModel.settings)
                    ]
                    (Ui.Button.default
                        { label = text "⚙"
                        , onPress =
                            Just <|
                                case model.settingsVisible of
                                    Visible ->
                                        GotSettingsVisible Hidden

                                    Hidden ->
                                        GotSettingsVisible Visible
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
            (el
                [ Font.color Ui.Theme.nearlyWhite
                , Background.color (rgba 0 0 0 0.4)
                , padding 4
                , Border.rounded 8
                , width fill
                , height fill
                ]
                options.body
            )
    }


viewMainMenu : View Msg
viewMainMenu =
    { title = "Hello Space!"
    , body =
        map MainMenuMessage <|
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
                    [ el [ centerX, Font.size 64 ] (text "Space Sim!")
                    , column
                        [ centerX, spacing 16 ]
                        [ Ui.Button.default
                            { onPress = Just ViewParticipate
                            , label = text "Participate"
                            }
                            |> el [ centerX ]
                        , Ui.Button.default
                            { onPress = Just ViewObserve
                            , label = text "Observe"
                            }
                            |> el [ centerX ]
                        , Ui.Button.default
                            { onPress = Nothing
                            , label = text "Load Simulation"
                            }
                            |> el [ centerX, Font.strike ]
                        ]
                    ]
                )
    }


viewParticipate : ParticipateModel -> View Msg
viewParticipate model =
    { title = "Hello Space! - Participate"
    , body =
        map ParticipateMessage <|
            column
                [ centerX
                , centerY
                , spacing 64
                ]
                [ text "Participate in the Simulation"
                    |> el [ centerX, Font.size 64, Font.underline ]
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


viewPlayerCivForm : ParticipateModel -> Element ParticipateMsg
viewPlayerCivForm model =
    column
        [ spacing 16
        , width fill
        ]
        [ Ui.Text.default
            []
            { onChange = SetNameSingular
            , text = model.civilizationNameSingular
            , label = Input.labelLeft [ width fill ] (text "Civilization Name Singular:")
            }
        , Ui.Text.default
            []
            { onChange = SetNamePlural
            , text = model.civilizationNamePlural
            , label = Input.labelLeft [ width fill ] (text "Civilization Name Plural:")
            }
        , Ui.Button.default
            { label =
                text <|
                    if model.hasUniquePluralName then
                        "Use '" ++ model.civilizationNameSingular ++ "' as the plural name"

                    else
                        "Use '" ++ model.civilizationNamePlural ++ "' as the plural name"
            , onPress = Just (ToggleNamePlural (not model.hasUniquePluralName))
            }
        , Ui.Text.default
            []
            { onChange = SetNamePossessive
            , text = model.civilizationNamePossessive
            , label = Input.labelLeft [ width fill ] (text "Civilization Name Possessive:")
            }
        , Ui.Button.default
            { label =
                text <|
                    if model.hasUniquePossessiveName then
                        "Use '" ++ model.civilizationNameSingular ++ "' as the possessive name"

                    else
                        "Use '" ++ model.civilizationNamePossessive ++ "' as the possessive name"
            , onPress = Just (ToggleNamePossessive (not model.hasUniquePossessiveName))
            }
        , Ui.Text.default
            []
            { onChange = SetHomePlanetName
            , text = model.homePlanetName
            , label = Input.labelLeft [ width fill ] (text "Home Planet Name:")
            }
        , Ui.Slider.int
            { onChange = GotMinSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Min Solar System Count: "
                        , displayGameValue "min-solar-system-count" (String.fromInt model.minSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 10000
            , value = model.minSolarSystemsToGenerate
            , step = Just 10
            }
        , Ui.Slider.int
            { onChange = GotMaxSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Max Solar System Count: "
                        , displayGameValue "max-solar-system-count" (String.fromInt model.maxSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 10000
            , value = model.maxSolarSystemsToGenerate
            , step = Just 10
            }
        , model.errors
            |> List.map viewError
            |> wrappedRow [ spacing 8 ]
        , Ui.Button.default
            { label = text "Start Game"
            , onPress = Just StartGame
            }
            |> el [ centerX ]
        ]


viewError : String -> Element msg
viewError error =
    el
        [ Background.color Ui.Theme.nearlyWhite
        , Font.color Ui.Theme.error
        , paddingXY 16 8
        , Border.rounded 32
        ]
        (text error)


viewExample : ParticipateModel -> Element ParticipateMsg
viewExample model =
    column
        [ spacing 8
        , alignTop
        , fill
            |> minimum 400
            |> maximum 600
            |> width
        ]
        [ text "Example:"
        , paragraph
            []
            [ text
                "As the battle rages on between the "
            , displayGameValue "plural-name-example" <|
                if model.hasUniquePluralName then
                    model.civilizationNamePlural

                else
                    model.civilizationNameSingular
            , text " and the Federation, the "
            , displayGameValue "singular-name-example" model.civilizationNameSingular
            , text " people begin to question the morality of continuing the war. But the "
            , displayGameValue "possessive-name-example" <|
                if model.hasUniquePossessiveName then
                    model.civilizationNamePossessive

                else
                    model.civilizationNameSingular
            , text " home planet, "
            , displayGameValue "home-planet-name-example" model.homePlanetName
            , text ", hangs in the balance."
            ]
        ]


displayGameValue : String -> String -> Element msg
displayGameValue id value =
    el
        [ Font.color (rgb 1 0 1)
        , Element.Extra.id id
        ]
        (text value)


viewObserve : ObserveModel -> View Msg
viewObserve model =
    { title = "Hello Space! - Observe"
    , body =
        map ObserveMessage <|
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
                    [ text "Observe the Simulation"
                        |> el [ centerX, Font.size 64, Font.underline ]
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


viewObserveForm : ObserveModel -> Element ObserveMsg
viewObserveForm model =
    column
        [ spacing 16
        , width fill
        ]
        [ Ui.Slider.int
            { onChange = OGotMinSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Min Solar System Count: "
                        , displayGameValue "min-solar-system-count" (String.fromInt model.minSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 10000
            , value = model.minSolarSystemsToGenerate
            , step = Just 10
            }
        , Ui.Slider.int
            { onChange = OGotMaxSolarSystemCount
            , label =
                Input.labelAbove []
                    (paragraph []
                        [ text "Max Solar System Count: "
                        , displayGameValue "max-solar-system-count" (String.fromInt model.maxSolarSystemsToGenerate)
                        ]
                    )
            , min = 10
            , max = 10000
            , value = model.maxSolarSystemsToGenerate
            , step = Just 10
            }

        -- , model.errors
        --     |> List.map viewError
        --     |> wrappedRow [ spacing 8 ]
        , Ui.Button.default
            { label = text "Begin Simulation"
            , onPress = Just BeginSimulation
            }
            |> el [ centerX ]
        ]
