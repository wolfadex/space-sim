module NewGame exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Random exposing (Seed)
import Shared exposing (Effect, Flags)
import SubCmd exposing (SubCmd)
import Ui.Button
import Ui.Text
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
    }


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
            ( model
            , SubCmd.effect
                (Shared.CreateGame
                    { name =
                        { singular = model.civilizationNameSingular
                        , possessive =
                            if model.hasUniquePossessiveName then
                                Just model.civilizationNamePossessive

                            else
                                Nothing
                        , many =
                            if model.hasUniquePluralName then
                                Just model.civilizationNameSingular

                            else
                                Nothing
                        }
                    , seed = model.seed
                    }
                )
            )


view : Model -> View Msg
view model =
    { title = "Hello Space!"
    , body =
        column
            [ centerX
            , centerY
            , spacing 64
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
                [ column
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
                    , Ui.Button.primary
                        { label = text "Start Game"
                        , onPress = Just StartGame
                        }
                        |> el [ centerX ]
                    ]
                , column
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
                ]
            ]
    }
