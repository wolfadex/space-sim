module App exposing
    ( CivilizationName
    , CivilizationReproductionRate
    , CivilizationSize
    , Focus(..)
    , Galaxy
    , Model
    , Msg
    , Planet
    , PlanetId
    , PlanetType
    , SolarSystem
    , SolarSystemId
    , Star
    , StarId
    , StarSize
    , exmptyGalaxy
    , init
    , subscriptions
    , update
    , view
    )

import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Id exposing (Id)
import IdDict exposing (IdDict)
import IdSet exposing (IdSet)
import Logic.Component exposing (Spec)
import Logic.Entity
import Logic.Entity.Extra
import Logic.System exposing (System)
import Random exposing (Generator, Seed)
import Random.Extra
import Shared exposing (Flags)
import Time
import View exposing (View)


type alias Model =
    { seed : Seed
    , galaxy : Galaxy
    , focus : Focus
    , ecsInternals : Logic.Entity.Extra.Internals
    , civilizationSizes : Logic.Component.Set CivilizationSize
    , civilizationNames : Logic.Component.Set CivilizationName
    , civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate
    }


type alias Galaxy =
    { solarSystems : IdDict SolarSystem
    , stars : IdDict Star
    , planets : IdDict Planet
    }


type alias SolarSystemId =
    Id SolarSystem


type alias StarId =
    Id Star


type alias PlanetId =
    Id Planet


type SolarSystem
    = SolarSystem SolarSystemData


type alias SolarSystemData =
    { stars : IdSet
    , planets : IdSet
    }


type Star
    = Star StarData


type alias StarData =
    { size : StarSize
    , solarSystem : SolarSystemId
    }


type Focus
    = FGalaxy
    | FSolarSystem SolarSystemId
    | FStar StarId
    | FPlanet PlanetId


type StarSize
    = Yellow
    | RedGiant
    | BlueGiant
    | WhiteDwarf
    | BlackDwarf


type Planet
    = Planet PlanetData


type alias PlanetData =
    { type_ : PlanetType
    , orbit : Int
    , solarSystem : SolarSystemId
    }


type PlanetType
    = Rocky
    | Gas



---- INIT ----


init : Flags -> ( Model, Effect Msg )
init flags =
    let
        initialWorld : Model
        initialWorld =
            { seed = Random.initialSeed flags.seed0
            , galaxy = exmptyGalaxy
            , focus = FGalaxy
            , ecsInternals = Logic.Entity.Extra.initInternals
            , civilizationSizes = Logic.Component.empty
            , civilizationNames = Logic.Component.empty
            , civilizationReproductionRates = Logic.Component.empty
            }

        ( _, finalWorld ) =
            Logic.Entity.Extra.create initialWorld
                |> Logic.Entity.with ( civilizationSizeSpec, 100 )
                |> Logic.Entity.with
                    ( civilizationNameSpec
                    , { singular = "Morlock"
                      , plural = "Morlocks"
                      }
                    )
                |> Logic.Entity.with ( civilizationReproductionRateSpec, 1.1 )
    in
    ( finalWorld, Effect.none )


civilizationReproductionRateSpec : Spec CivilizationReproductionRate { world | civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate }
civilizationReproductionRateSpec =
    Logic.Component.Spec .civilizationReproductionRates (\comps world -> { world | civilizationReproductionRates = comps })


type alias CivilizationReproductionRate =
    Float


civilizationSizeSpec : Spec CivilizationSize { world | civilizationSizes : Logic.Component.Set CivilizationSize }
civilizationSizeSpec =
    Logic.Component.Spec .civilizationSizes (\comps world -> { world | civilizationSizes = comps })


type alias CivilizationSize =
    Float


civilizationNameSpec : Spec CivilizationName { world | civilizationNames : Logic.Component.Set CivilizationName }
civilizationNameSpec =
    Logic.Component.Spec .civilizationNames (\comps world -> { world | civilizationNames = comps })


type alias CivilizationName =
    { singular : String
    , plural : String
    }


exmptyGalaxy : Galaxy
exmptyGalaxy =
    { solarSystems = IdDict.empty
    , stars = IdDict.empty
    , planets = IdDict.empty
    }



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 3000 (\_ -> Tick)


type Msg
    = GenerateGlaxy
    | DeleteGalaxy
    | SetFocus Focus
    | Tick


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GenerateGlaxy ->
            let
                ( galaxy, seed ) =
                    Random.step generateGalaxy model.seed
            in
            ( { model
                | seed = seed
                , galaxy =
                    { solarSystems = galaxy.solarSystems
                    , stars = galaxy.stars
                    , planets = galaxy.planets
                    }
              }
            , Effect.none
            )

        DeleteGalaxy ->
            ( { model | galaxy = exmptyGalaxy, focus = FGalaxy }
            , Effect.none
            )

        SetFocus focus ->
            ( { model | focus = focus }, Effect.none )

        Tick ->
            ( birthSystem civilizationReproductionRateSpec civilizationSizeSpec model
            , Effect.none
            )


birthSystem : Spec CivilizationReproductionRate world -> Spec CivilizationSize world -> System world
birthSystem =
    Logic.System.step2
        (\( reproductionRate, _ ) ( populationSize, setPopulationSize ) ->
            setPopulationSize (reproductionRate * populationSize)
        )


generateGalaxy : Generator Galaxy
generateGalaxy =
    Random.int 10 20
        |> Random.andThen
            (List.range 0
                >> List.map
                    (\_ ->
                        Random.andThen generateSolarSystem
                            Id.generate
                    )
                >> Random.Extra.combine
            )
        |> Random.map
            (List.foldl
                (\galaxy finalGalaxy ->
                    { solarSystems = IdDict.union finalGalaxy.solarSystems galaxy.solarSystems
                    , stars = IdDict.union finalGalaxy.stars galaxy.stars
                    , planets = IdDict.union finalGalaxy.planets galaxy.planets
                    }
                )
                exmptyGalaxy
            )


generateSolarSystem : SolarSystemId -> Generator Galaxy
generateSolarSystem solarSystemId =
    Random.map2
        (\stars planets ->
            { solarSystems =
                IdDict.singleton solarSystemId
                    (SolarSystem
                        { stars = IdSet.fromList (IdDict.keys stars)
                        , planets = IdSet.fromList (IdDict.keys planets)
                        }
                    )
            , stars = stars
            , planets = planets
            }
        )
        (generateMinMax 1 3 (generateStar solarSystemId)
            |> Random.map IdDict.fromList
        )
        (generateMinMax 1 12 (generatePlanet solarSystemId)
            |> Random.map IdDict.fromList
        )


generateStar : SolarSystemId -> Generator ( StarId, Star )
generateStar solarSystemId =
    Random.map2
        (\id size ->
            ( id
            , Star
                { size = size
                , solarSystem = solarSystemId
                }
            )
        )
        Id.generate
        (Random.uniform Yellow
            [ RedGiant
            , BlueGiant
            , WhiteDwarf
            , BlackDwarf
            ]
        )


generatePlanet : SolarSystemId -> Generator ( PlanetId, Planet )
generatePlanet solarSystemId =
    Random.map3
        (\id type_ orbit ->
            ( id
            , Planet
                { type_ = type_
                , orbit = orbit
                , solarSystem = solarSystemId
                }
            )
        )
        Id.generate
        (Random.uniform Rocky [ Gas ])
        (Random.int 0 12)


generateMinMax : Int -> Int -> Generator a -> Generator (List a)
generateMinMax min max generator =
    Random.andThen
        (\count -> Random.list count generator)
        (Random.int min max)



---- VIEW ----


view : Model -> View Msg
view model =
    { title = "Hello, Elm!"
    , body =
        column
            []
            [ Input.button
                []
                { label = text "Generate"
                , onPress = Just GenerateGlaxy
                }
            , Input.button
                []
                { label = text "Delete"
                , onPress = Just DeleteGalaxy
                }
            , case model.focus of
                FGalaxy ->
                    viewGalaxy model.galaxy

                FSolarSystem id ->
                    case IdDict.get id model.galaxy.solarSystems of
                        Nothing ->
                            text "Missing solar system"

                        Just solarSystem ->
                            viewSlice (viewSolarSystem model.galaxy ( id, solarSystem ))

                FStar starId ->
                    case IdDict.get starId model.galaxy.stars of
                        Nothing ->
                            text "Missing star"

                        Just star ->
                            viewSlice (viewBody (\(Star s) -> s) ( starId, star ) viewStar)

                FPlanet planetId ->
                    case IdDict.get planetId model.galaxy.planets of
                        Nothing ->
                            text "Missing planet"

                        Just planet ->
                            viewSlice (viewBody (\(Planet p) -> p) ( planetId, planet ) viewPlanet)
            ]
    }


viewSlice : Element Msg -> Element Msg
viewSlice slice =
    column
        []
        [ Input.button
            []
            { label = text "View Galaxy"
            , onPress = Just (SetFocus FGalaxy)
            }
        , slice
        ]


viewBody : (a -> { r | solarSystem : SolarSystemId }) -> ( Id a, a ) -> (( Id a, a ) -> Element Msg) -> Element Msg
viewBody fn (( _, body ) as arg) bodyFn =
    column
        [ spacing 8 ]
        [ Input.button
            []
            { label = text "View System"
            , onPress =
                fn body
                    |> .solarSystem
                    |> FSolarSystem
                    |> SetFocus
                    |> Just
            }
        , bodyFn arg
        ]


viewGalaxy : Galaxy -> Element Msg
viewGalaxy galaxy =
    IdDict.toList galaxy.solarSystems
        |> List.map (viewSolarSystem galaxy)
        |> column []


viewSolarSystem : Galaxy -> ( SolarSystemId, SolarSystem ) -> Element Msg
viewSolarSystem galaxy ( solarSystemId, SolarSystem solarSystem ) =
    let
        getCelestialBodies : (Galaxy -> IdDict a) -> IdSet -> List ( Id a, a )
        getCelestialBodies bodyType =
            IdSet.toList
                >> List.filterMap
                    (\bodyId ->
                        IdDict.get bodyId (bodyType galaxy)
                            |> Maybe.map (Tuple.pair bodyId)
                    )

        stars : List ( StarId, Star )
        stars =
            getCelestialBodies .stars solarSystem.stars

        planets : List ( PlanetId, Planet )
        planets =
            getCelestialBodies .planets solarSystem.planets
    in
    column
        [ padding 8 ]
        [ Input.button
            []
            { label = text "Solar System:"
            , onPress = Just (SetFocus (FSolarSystem solarSystemId))
            }
        , column [ padding 8 ]
            [ text "Stars:"
            , stars
                |> List.map viewStar
                |> column [ padding 8 ]
            ]
        , column [ padding 8 ]
            [ text "Planets:"
            , planets
                |> List.sortBy (\( _, Planet { orbit } ) -> orbit)
                |> List.map viewPlanet
                |> column [ padding 8 ]
            ]
        ]


viewStar : ( StarId, Star ) -> Element Msg
viewStar ( id, Star star ) =
    Input.button
        []
        { label =
            text <|
                case star.size of
                    Yellow ->
                        "Yellow"

                    RedGiant ->
                        "Red Giant"

                    BlueGiant ->
                        "Blue Giant"

                    WhiteDwarf ->
                        "White Dwarf"

                    BlackDwarf ->
                        "Black Dwarf"
        , onPress = Just (SetFocus (FStar id))
        }


viewPlanet : ( PlanetId, Planet ) -> Element Msg
viewPlanet ( id, Planet planet ) =
    Input.button
        []
        { label =
            text <|
                case planet.type_ of
                    Rocky ->
                        "Rocky"

                    Gas ->
                        "Gas"
        , onPress = Just (SetFocus (FPlanet id))
        }
