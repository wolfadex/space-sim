module NewGame exposing
    ( MainMenuModel
    , Model(..)
    , Msg
    , ObserveModel
    , ParticipateModel
    , init
    , update
    , view
    )

import Data.Names exposing (CivilizationName)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Game.Components exposing (Visible(..))
import Shared
    exposing
        ( Effect(..)
        , PlayType(..)
        , SharedMsg
        , SharedModel
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
    ( MainMenu {}
    , SubCmd.none
    )


type Model
    = MainMenu MainMenuModel
    | Participate ParticipateModel
    | Observe ObserveModel


type alias MainMenuModel =
    {}


type alias ParticipateModel =
    { settingsVisible : Visible
    , civilizationNameSingular : String
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
    { settingsVisible = Hidden
    , civilizationNameSingular = ""
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


type Msg
    = MainMenuMessage MainMenuMsg
    | ParticipateMessage ParticipateMsg
    | ObserveMessage ObserveMsg


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
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg
    | GotMinSolarSystemCount Int
    | GotMaxSolarSystemCount Int
    | ViewMainFromParticipate


type ObserveMsg
    = OGotMinSolarSystemCount Int
    | OGotMaxSolarSystemCount Int
    | BeginSimulation
    | ViewMainFromObserve


update : SharedModel -> Msg -> Model -> ( Model, SubCmd Msg Effect )
update _ msg model =
    case ( msg, model ) of
        ( MainMenuMessage message, MainMenu mod ) ->
            updateMainMenu message mod

        ( ParticipateMessage message, Participate mod ) ->
            updateParticipate message mod

        ( ObserveMessage message, Observe mod ) ->
            updateObserve message mod

        _ ->
            ( model, SubCmd.none )


updateMainMenu : MainMenuMsg -> MainMenuModel -> ( Model, SubCmd Msg Effect )
updateMainMenu msg _ =
    case msg of
        ViewParticipate ->
            ( Participate baseParticipateModel, SubCmd.none )

        ViewObserve ->
            ( Observe baseObserveModel, SubCmd.none )


updateParticipate : ParticipateMsg -> ParticipateModel -> ( Model, SubCmd Msg Effect )
updateParticipate msg model =
    case msg of
        ViewMainFromParticipate ->
            ( MainMenu {}, SubCmd.none )

        SetNameSingular singular ->
            ( Participate { model | civilizationNameSingular = singular }
            , SubCmd.none
            )

        SetNamePlural plural ->
            ( Participate { model | civilizationNamePlural = plural }
            , SubCmd.none
            )

        ToggleNamePlural enabled ->
            ( Participate { model | hasUniquePluralName = enabled }
            , SubCmd.none
            )

        SetNamePossessive possessive ->
            ( Participate { model | civilizationNamePossessive = possessive }
            , SubCmd.none
            )

        ToggleNamePossessive enabled ->
            ( Participate { model | hasUniquePossessiveName = enabled }
            , SubCmd.none
            )

        SetHomePlanetName name ->
            ( Participate { model | homePlanetName = name }
            , SubCmd.none
            )

        GotMinSolarSystemCount minCount ->
            ( Participate
                { model
                    | minSolarSystemsToGenerate = minCount
                    , maxSolarSystemsToGenerate = max minCount model.maxSolarSystemsToGenerate
                }
            , SubCmd.none
            )

        GotMaxSolarSystemCount maxCount ->
            ( Participate
                { model
                    | minSolarSystemsToGenerate = min model.minSolarSystemsToGenerate maxCount
                    , maxSolarSystemsToGenerate = maxCount
                }
            , SubCmd.none
            )

        StartGame ->
            case Validator.run createGameValidator model of
                Ok ( validName, validHomeName ) ->
                    ( Participate model
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
                    ( Participate { model | errors = errs }, SubCmd.none )

        GotSettingsVisible visible ->
            ( Participate { model | settingsVisible = visible }, SubCmd.none )

        GotLocalSharedMessage settingsChange ->
            ( Participate model
            , SubCmd.effect (GotSharedMessage settingsChange)
            )


updateObserve : ObserveMsg -> ObserveModel -> ( Model, SubCmd Msg Effect )
updateObserve msg model =
    case msg of
        ViewMainFromObserve ->
            ( MainMenu {}, SubCmd.none )

        OGotMinSolarSystemCount minCount ->
            ( Observe
                { model
                    | minSolarSystemsToGenerate = minCount
                    , maxSolarSystemsToGenerate = max minCount model.maxSolarSystemsToGenerate
                }
            , SubCmd.none
            )

        OGotMaxSolarSystemCount maxCount ->
            ( Observe
                { model
                    | minSolarSystemsToGenerate = min model.minSolarSystemsToGenerate maxCount
                    , maxSolarSystemsToGenerate = maxCount
                }
            , SubCmd.none
            )

        BeginSimulation ->
            ( Observe model
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
    case model of
        MainMenu m ->
            viewMain m

        Participate m ->
            viewParticipate sharedModel m

        Observe m ->
            viewObserve sharedModel m


viewMain : MainMenuModel -> View Msg
viewMain _ =
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
                    [ text "Space Sim!"
                        |> el [ centerX, Font.size 64 ]
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


viewParticipate : SharedModel -> ParticipateModel -> View Msg
viewParticipate sharedModel model =
    { title = "Hello Space! - Participate"
    , body =
        map ParticipateMessage <|
            el
                [ padding 16
                , width fill
                , height fill
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
                    (el
                        [ padding 16 ]
                        (Ui.Button.default
                            { label = text "Main Menu"
                            , onPress = Just ViewMainFromParticipate
                            }
                        )
                    )
                ]
                (column
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
                )
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
            , label = Input.labelAbove [] (text ("Min Solar System Count: " ++ String.fromInt model.minSolarSystemsToGenerate))
            , min = 10
            , max = 10000
            , value = model.minSolarSystemsToGenerate
            , step = Just 10
            }
        , Ui.Slider.int
            { onChange = GotMaxSolarSystemCount
            , label =
                Input.labelAbove []
                    (column []
                        [ text ("Max Solar System Count: " ++ String.fromInt model.maxSolarSystemsToGenerate)
                        , el [ Font.size 12 ] (text "ℹ️ The Milky Way has over 3200 solar systems")
                        , el [ Font.size 12 ] (text "ℹ️ Higher amounts will require a more powerful computer")
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
        , Ui.Button.primary
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
            , el
                [ Font.color (rgb 1 0 1)
                , Element.Extra.id "plural-name-example"
                ]
                (text <|
                    if model.hasUniquePluralName then
                        model.civilizationNamePlural

                    else
                        model.civilizationNameSingular
                )
            , text " and the Federation, the "
            , el
                [ Font.color (rgb 1 0 1)
                , Element.Extra.id "singular-name-example"
                ]
                (text model.civilizationNameSingular)
            , text " people begin to question the morality of continuing the war. But the "
            , el
                [ Font.color (rgb 1 0 1)
                , Element.Extra.id "possessive-name-example"
                ]
                (text <|
                    if model.hasUniquePossessiveName then
                        model.civilizationNamePossessive

                    else
                        model.civilizationNameSingular
                )
            , text " home planet, "
            , el
                [ Font.color (rgb 1 0 1)
                , Element.Extra.id "home-planet-name-example"
                ]
                (text model.homePlanetName)
            , text ", hangs in the balance."
            ]
        ]


viewObserve : SharedModel -> ObserveModel -> View Msg
viewObserve _ model =
    { title = "Hello Space! - Observe"
    , body =
        map ObserveMessage <|
            el
                [ padding 16
                , width fill
                , height fill
                , inFront
                    (el
                        [ padding 16 ]
                        (Ui.Button.default
                            { label = text "Main Menu"
                            , onPress = Just ViewMainFromObserve
                            }
                        )
                    )
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
            , label = Input.labelAbove [] (text ("Min Solar System Count: " ++ String.fromInt model.minSolarSystemsToGenerate))
            , min = 10
            , max = 10000
            , value = model.minSolarSystemsToGenerate
            , step = Just 10
            }
        , Ui.Slider.int
            { onChange = OGotMaxSolarSystemCount
            , label =
                Input.labelAbove []
                    (column []
                        [ text ("Max Solar System Count: " ++ String.fromInt model.maxSolarSystemsToGenerate)
                        , el [ Font.size 12 ] (text "ℹ️ The Milky Way has over 3200 solar systems")
                        , el [ Font.size 12 ] (text "ℹ️ Higher amounts will require a more powerful computer")
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
        , Ui.Button.primary
            { label = text "Begin Simulation"
            , onPress = Just BeginSimulation
            }
            |> el [ centerX ]
        ]
