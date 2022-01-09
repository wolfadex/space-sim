module App exposing
    ( Focus(..)
    , Galaxy
    , Model
    , Msg
    , Planet
    , PlanetType
    , SolarSystem
    , Star
    , StarSize
    , exmptyGalaxy
    , init
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Random exposing (Generator, Seed)
import Random.Extra
import Set exposing (Set)
import Shared exposing (Flags)
import View exposing (View)


type alias Model =
    { seed : Seed
    , galaxy : Galaxy
    , focus : Focus
    }


type alias Galaxy =
    { solarSystems : Dict Int SolarSystem
    , stars : Dict ( Int, Int ) Star
    , planets : Dict ( Int, Int ) Planet
    }


type alias SolarSystem =
    { stars : Set Int
    , planets : Set Int
    }


type alias Star =
    { size : StarSize
    }


type Focus
    = FGalaxy
    | FSolarSystem Int
    | FStar ( Int, Int )
    | FPlanet ( Int, Int )


type StarSize
    = Yellow
    | RedGiant
    | BlueGiant
    | WhiteDwarf
    | BlackDwarf


type alias Planet =
    { type_ : PlanetType
    , orbit : Int
    }


type PlanetType
    = Rocky
    | Gas



---- INIT ----


init : Flags -> ( Model, Effect Msg )
init flags =
    ( { seed = Random.initialSeed flags.seed0
      , galaxy = exmptyGalaxy
      , focus = FGalaxy
      }
    , Effect.none
    )


exmptyGalaxy : Galaxy
exmptyGalaxy =
    { solarSystems = Dict.empty
    , stars = Dict.empty
    , planets = Dict.empty
    }



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = GenerateGlaxy
    | DeleteGalaxy
    | SetFocus Focus


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
            ( { model | galaxy = exmptyGalaxy }
            , Effect.none
            )

        SetFocus focus ->
            ( { model | focus = focus }, Effect.none )


generateGalaxy : Generator Galaxy
generateGalaxy =
    Random.int 10 20
        |> Random.andThen
            (List.range 0
                >> List.map generateSolarSystem
                >> Random.Extra.combine
            )
        |> Random.map
            (List.foldl
                (\galaxy finalGalaxy ->
                    { solarSystems = Dict.union finalGalaxy.solarSystems galaxy.solarSystems
                    , stars = Dict.union finalGalaxy.stars galaxy.stars
                    , planets = Dict.union finalGalaxy.planets galaxy.planets
                    }
                )
                exmptyGalaxy
            )


generateSolarSystem : Int -> Generator Galaxy
generateSolarSystem solarSystemId =
    Random.map2
        (\stars planets ->
            { solarSystems =
                Dict.singleton solarSystemId
                    { stars = Dict.keys stars |> List.map Tuple.second |> Set.fromList
                    , planets = Dict.keys planets |> List.map Tuple.second |> Set.fromList
                    }
            , stars = stars
            , planets = planets
            }
        )
        (generateRange 1 3 (generateStar solarSystemId)
            |> Random.map Dict.fromList
        )
        (generateRange 1 12 (generatePlanet solarSystemId)
            |> Random.map Dict.fromList
        )


generateStar : Int -> Int -> Generator ( ( Int, Int ), Star )
generateStar solarSystemId id =
    Random.map
        (\size ->
            ( ( solarSystemId, id ), { size = size } )
        )
        (Random.uniform Yellow
            [ RedGiant
            , BlueGiant
            , WhiteDwarf
            , BlackDwarf
            ]
        )


generatePlanet : Int -> Int -> Generator ( ( Int, Int ), Planet )
generatePlanet solarSystemId id =
    Random.map2
        (\type_ orbit ->
            ( ( solarSystemId, id )
            , { type_ = type_
              , orbit = orbit
              }
            )
        )
        (Random.uniform Rocky [ Gas ])
        (Random.int 0 12)


generateRange : Int -> Int -> (Int -> Generator a) -> Generator (List a)
generateRange min max generator =
    Random.andThen
        (List.range 0 >> List.map generator >> Random.Extra.combine)
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
                    case Dict.get id model.galaxy.solarSystems of
                        Nothing ->
                            text "Missing solar system"

                        Just solarSystem ->
                            viewSlice (viewSolarSystem model.galaxy ( id, solarSystem ))

                FStar id ->
                    case Dict.get id model.galaxy.stars of
                        Nothing ->
                            text "Missing star"

                        Just star ->
                            viewSlice (viewBody ( id, star ) viewStar)

                FPlanet id ->
                    case Dict.get id model.galaxy.planets of
                        Nothing ->
                            text "Missing planet"

                        Just planet ->
                            viewSlice (viewBody ( id, planet ) viewPlanet)
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


viewBody : ( ( Int, Int ), a ) -> (( ( Int, Int ), a ) -> Element Msg) -> Element Msg
viewBody (( ( solarSystemId, _ ), _ ) as body) bodyFn =
    column
        [ spacing 8 ]
        [ Input.button
            []
            { label = text "View System"
            , onPress = Just (SetFocus (FSolarSystem solarSystemId))
            }
        , bodyFn body
        ]


viewGalaxy : Galaxy -> Element Msg
viewGalaxy galaxy =
    Dict.toList galaxy.solarSystems
        |> List.map (viewSolarSystem galaxy)
        |> column []


viewSolarSystem : Galaxy -> ( Int, SolarSystem ) -> Element Msg
viewSolarSystem galaxy ( solarSystemId, solarSystem ) =
    let
        getCelestialBody : (Galaxy -> Dict ( Int, Int ) a) -> Set Int -> List ( ( Int, Int ), a )
        getCelestialBody bodyType =
            Set.toList
                >> List.filterMap
                    (\bodyId ->
                        let
                            id : ( Int, Int )
                            id =
                                ( solarSystemId, bodyId )
                        in
                        Dict.get id (bodyType galaxy)
                            |> Maybe.map (Tuple.pair id)
                    )

        stars : List ( ( Int, Int ), Star )
        stars =
            getCelestialBody .stars solarSystem.stars

        planets : List ( ( Int, Int ), Planet )
        planets =
            getCelestialBody .planets solarSystem.planets
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
                |> List.sortBy (\( _, { orbit } ) -> orbit)
                |> List.map viewPlanet
                |> column [ padding 8 ]
            ]
        ]


viewStar : ( ( Int, Int ), Star ) -> Element Msg
viewStar ( id, star ) =
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


viewPlanet : ( ( Int, Int ), Planet ) -> Element Msg
viewPlanet ( id, planet ) =
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
