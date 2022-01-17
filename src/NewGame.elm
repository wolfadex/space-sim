module NewGame exposing (Model, Msg, baseNewGameModel, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Game.Components exposing (Name)
import Random exposing (Seed)
import Shared exposing (Effect)
import SubCmd exposing (SubCmd)
import Ui.Button
import Ui.Text
import Ui.Theme
import Validator exposing (Validator)
import View exposing (View)



---- INIT ----


init : Seed -> ( Model, SubCmd Msg Effect )
init seed =
    ( { baseNewGameModel | seed = seed }
    , SubCmd.none
    )


type alias Model =
    { seed : Seed
    , civilizationNameSingular : String
    , civilizationNamePlural : String
    , hasUniquePluralName : Bool
    , civilizationNamePossessive : String
    , hasUniquePossessiveName : Bool
    , homePlanetName : String
    , errors : List String
    }


baseNewGameModel : Model
baseNewGameModel =
    { seed = Random.initialSeed 0
    , civilizationNameSingular = ""
    , civilizationNamePlural = ""
    , hasUniquePluralName = True
    , civilizationNamePossessive = ""
    , hasUniquePossessiveName = True
    , homePlanetName = ""
    , errors = []
    }



---- UPDATE ----


type Msg
    = SetNameSingular String
    | SetNamePlural String
    | ToggleNamePlural Bool
    | SetNamePossessive String
    | ToggleNamePossessive Bool
    | StartGame
    | SetHomePlanetName String


update : Msg -> Model -> ( Model, SubCmd Msg Effect )
update msg model =
    case msg of
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

        StartGame ->
            case Validator.run createGameValidator model of
                Ok ( validName, validHomeName ) ->
                    ( model
                    , SubCmd.effect
                        (Shared.CreateGame
                            { name = validName
                            , homePlanetName = validHomeName
                            , seed = model.seed
                            }
                        )
                    )

                Err errs ->
                    ( { model | errors = errs }, SubCmd.none )


createGameValidator : Validator Model String ( Name, String )
createGameValidator =
    Validator.map2 Tuple.pair
        civNameValidator
        homeNameValidator


homeNameValidator : Validator Model String String
homeNameValidator =
    Validator.succeed identity
        |> Validator.required .homePlanetName String.isEmpty "Home planet name is required" (Validator.custom Ok)


civNameValidator : Validator Model String Name
civNameValidator =
    Validator.succeed Name
        |> Validator.required .civilizationNameSingular String.isEmpty "Singular name is required" (Validator.custom Ok)
        |> Validator.required identity (\_ -> False) "" (Validator.custom pluralNameValidator)
        |> Validator.required identity (\_ -> False) "" (Validator.custom possessiveNameValidator)


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


view : Model -> View Msg
view model =
    { title = "Hello Space!"
    , body =
        column
            [ centerX
            , centerY
            , spacing 64
            , padding 16
            ]
            [ text "Space Sim!"
                |> el [ centerX, Font.size 64 ]
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
        , model.errors
            |> List.map viewError
            |> wrappedRow [ spacing 8 ]
        , Ui.Button.primary
            { label = text "Start Game"
            , onPress = Just StartGame
            }
            |> el [ centerX ]
        ]


viewError : String -> Element Msg
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
