module App exposing
    ( CelestialBodyForm
    , CivilizationReproductionRate
    , CivilizationSize
    , Focus(..)
    , Galaxy
    , Model
    , Msg
    , Name
    , Orbit
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
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Shared exposing (Flags)
import Time
import View exposing (View)


type alias Model =
    { seed : Seed
    , focus : Focus

    -- ECS stuff
    , ecsInternals : Logic.Entity.Extra.Internals
    , civilizationSizes : Logic.Component.Set CivilizationSize
    , named : Logic.Component.Set Name
    , civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate
    , celestialBodyForms : Logic.Component.Set CelestialBodyForm
    , starForms : Logic.Component.Set StarSize
    , parents : Logic.Component.Set EntityID
    , children : Logic.Component.Set (Set EntityID)
    , orbits : Logic.Component.Set Orbit

    -- Book keeping for plants, stars, and their grouping (solar systems)
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    }


type alias Galaxy =
    { planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    }


type Focus
    = FGalaxy
    | FSolarSystem EntityID
    | FStar EntityID
    | FPlanet EntityID



---- INIT ----


init : Flags -> ( Model, Effect Msg )
init flags =
    let
        initialWorld : Model
        initialWorld =
            { seed = Random.initialSeed flags.seed0
            , focus = FGalaxy
            , ecsInternals = Logic.Entity.Extra.initInternals
            , civilizationSizes = Logic.Component.empty
            , named = Logic.Component.empty
            , civilizationReproductionRates = Logic.Component.empty
            , celestialBodyForms = Logic.Component.empty
            , starForms = Logic.Component.empty
            , parents = Logic.Component.empty
            , children = Logic.Component.empty
            , orbits = Logic.Component.empty
            , planets = Set.empty
            , stars = Set.empty
            , solarSystems = Set.empty
            }

        ( _, finalWorld ) =
            Logic.Entity.Extra.create initialWorld
                |> Logic.Entity.with ( civilizationSizeSpec, 100 )
                |> Logic.Entity.with
                    ( namedSpec
                    , { singular = "Morlock"
                      , plural = Just "Morlocks"
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


namedSpec : Spec Name { world | named : Logic.Component.Set Name }
namedSpec =
    Logic.Component.Spec .named (\comps world -> { world | named = comps })


type alias Name =
    { singular : String
    , plural : Maybe String
    }


celestialBodySpec : Spec CelestialBodyForm { world | celestialBodyForms : Logic.Component.Set CelestialBodyForm }
celestialBodySpec =
    Logic.Component.Spec .celestialBodyForms (\comps world -> { world | celestialBodyForms = comps })


type CelestialBodyForm
    = Rocky
    | Gas


orbitSpec : Spec Orbit { world | orbits : Logic.Component.Set Orbit }
orbitSpec =
    Logic.Component.Spec .orbits (\comps world -> { world | orbits = comps })


type alias Orbit =
    Int


starFormSpec : Spec StarSize { world | starForms : Logic.Component.Set StarSize }
starFormSpec =
    Logic.Component.Spec .starForms (\comps world -> { world | starForms = comps })


type StarSize
    = Yellow
    | RedGiant
    | BlueGiant
    | WhiteDwarf
    | BlackDwarf


parentSpec : Spec EntityID { world | parents : Logic.Component.Set EntityID }
parentSpec =
    Logic.Component.Spec .parents (\comps world -> { world | parents = comps })


childrenSpec : Spec (Set EntityID) { world | children : Logic.Component.Set (Set EntityID) }
childrenSpec =
    Logic.Component.Spec .children (\comps world -> { world | children = comps })


exmptyGalaxy : Galaxy
exmptyGalaxy =
    { solarSystems = Set.empty
    , stars = Set.empty
    , planets = Set.empty
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
                ( modelWithNewEntities, seed ) =
                    Random.step (generateGalaxy model) model.seed
            in
            ( { modelWithNewEntities | seed = seed }
            , Effect.none
            )

        DeleteGalaxy ->
            -- ( { model | galaxy = exmptyGalaxy, focus = FGalaxy }
            -- , Effect.none
            -- )
            Debug.todo "TODO"

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


generateGalaxy : Model -> Generator Model
generateGalaxy model =
    generateManyEntities 10 20 model generateSolarSystem
        |> Random.map Tuple.second


generateSolarSystem : ( EntityID, Model ) -> Generator ( EntityID, Model )
generateSolarSystem ( solarSystemId, world ) =
    generateManyEntities 1 3 world (generateStar solarSystemId)
        |> Random.andThen
            (\( starIds, starWorld ) ->
                Random.map
                    (\( planetIds, finalWorld ) ->
                        ( solarSystemId
                        , { finalWorld | solarSystems = Set.insert solarSystemId finalWorld.solarSystems }
                        )
                            |> Logic.Entity.with ( childrenSpec, Set.union planetIds starIds )
                    )
                    (generateManyEntities 1 12 starWorld (generatePlanet solarSystemId))
            )


generateStar : EntityID -> ( EntityID, Model ) -> Generator ( EntityID, Model )
generateStar solarSystemId ( starId, world ) =
    Random.map
        (\size ->
            ( starId, world )
                |> Logic.Entity.with ( starFormSpec, size )
                |> Logic.Entity.with ( parentSpec, solarSystemId )
                |> Tuple.mapSecond (\w -> { w | stars = Set.insert starId w.stars })
        )
        (Random.uniform Yellow
            [ RedGiant
            , BlueGiant
            , WhiteDwarf
            , BlackDwarf
            ]
        )


generatePlanet : EntityID -> ( EntityID, Model ) -> Generator ( EntityID, Model )
generatePlanet solarSystemId ( planetId, world ) =
    Random.uniform Rocky
        [ Gas
        ]
        |> Random.andThen
            (\planetType ->
                Random.map
                    (\orbit ->
                        ( planetId, world )
                            |> Logic.Entity.with ( celestialBodySpec, planetType )
                            |> Logic.Entity.with ( orbitSpec, orbit )
                            |> Logic.Entity.with ( parentSpec, solarSystemId )
                            |> Tuple.mapSecond (\w -> { w | planets = Set.insert planetId w.planets })
                    )
                    (case planetType of
                        Rocky ->
                            Random.int 0 7

                        Gas ->
                            Random.int 5 12
                    )
            )



-- Random.uniform Rocky [ Gas ]
--     |> Random.andThen
--         (\type_ ->
--             Random.map4
--                 (\id orbit radius percentWater ->
--                     ( id
--                     , Planet
--                         { type_ = type_
--                         , orbit = orbit
--                         , solarSystem = solarSystemId
--                         , radius = radius
--                         , percentWater = percentWater
--                         }
--                     )
--                 )
--                 Id.generate
--                 (case type_ of
--                     Rocky ->
--                         Random.int 0 7
--                     Gas ->
--                         Random.int 5 12
--                 )
--                 (generatePlanetRadius type_)
--                 generatePlanetWaterPercent
--         )


{-| Generate the amount of water on a planet. For a Gas planet this would be water vapor.
-}
generatePlanetWaterPercent : Generator Float
generatePlanetWaterPercent =
    Random.float 0.0 100


{-| Generate the radius of a planet based on its type.

The `Rocky` radius is based on exaggerated Mercurey and Earth
The 'Gas' radius is based on exaggerated Neptude and Jupiter

-}
generatePlanetRadius : CelestialBodyForm -> Generator Float
generatePlanetRadius type_ =
    case type_ of
        Rocky ->
            Random.float 1000.0 8000.0

        Gas ->
            Random.float 22000.0 90000.0


{-| Generate a random number of entities between minimum and maximum.
Order of minimum and maximum doesn't matter as the function will sort the values.
-}
generateManyEntities : Int -> Int -> Model -> (( EntityID, Model ) -> Generator ( EntityID, Model )) -> Generator ( Set EntityID, Model )
generateManyEntities minimum maximum world fn =
    Random.int (min minimum maximum) (max minimum maximum)
        |> Random.andThen
            (\count ->
                List.foldl
                    (\_ ->
                        Random.andThen
                            (\( ids, nextWorld ) ->
                                Random.map (Tuple.mapFirst (\id -> Set.insert id ids))
                                    (generateEntity nextWorld fn)
                            )
                    )
                    (Random.constant ( Set.empty, world ))
                    (List.range 1 count)
            )


generateEntity : Model -> (( EntityID, Model ) -> Generator ( EntityID, Model )) -> Generator ( EntityID, Model )
generateEntity world fn =
    fn (Logic.Entity.Extra.create world)



---- VIEW ----


view : Model -> View Msg
view model =
    { title = "Hello Space!"
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
                    viewGalaxy model

                FSolarSystem id ->
                    if Set.member id model.solarSystems then
                        viewSlice (viewSolarSystem model id)

                    else
                        text "Missing solar system"

                FStar starId ->
                    if Set.member starId model.stars then
                        viewSlice (viewBody model viewStar starId)

                    else
                        text "Missing star"

                FPlanet planetId ->
                    if Set.member planetId model.planets then
                        viewSlice (viewBody model viewPlanet planetId)

                    else
                        text "Missing planet"
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


viewBody : Model -> (Model -> EntityID -> Element Msg) -> EntityID -> Element Msg
viewBody model bodyFn id =
    column
        [ spacing 8 ]
        [ Input.button
            []
            { label = text "View System"
            , onPress =
                Maybe.map (FSolarSystem >> SetFocus)
                    (Logic.Component.get id model.parents)
            }
        , bodyFn model id
        ]


viewGalaxy : Model -> Element Msg
viewGalaxy model =
    Set.toList model.solarSystems
        |> List.map (viewSolarSystem model)
        |> column []


viewSolarSystem : Model -> EntityID -> Element Msg
viewSolarSystem model solarSystemId =
    let
        ( stars, planets ) =
            Logic.Component.get solarSystemId model.children
                |> Maybe.map
                    (\children ->
                        ( Set.intersect children model.stars
                        , Set.intersect children model.planets
                        )
                    )
                |> Maybe.withDefault ( Set.empty, Set.empty )
    in
    column
        [ padding 8 ]
        [ Input.button
            []
            { label = text ("Solar System (" ++ String.fromInt solarSystemId ++ ")")
            , onPress = Just (SetFocus (FSolarSystem solarSystemId))
            }
        , column [ padding 8 ]
            [ text "Stars:"
            , stars
                |> Set.toList
                |> List.map (viewStar model)
                |> column [ padding 8 ]
            ]
        , column [ padding 8 ]
            [ text "Planets:"
            , planets
                |> Set.toList
                |> List.filterMap (\planetId -> Maybe.map (Tuple.pair planetId) (Logic.Component.get planetId model.orbits))
                |> List.sortBy (\( _, orbit ) -> orbit)
                |> List.map (Tuple.first >> viewPlanet model)
                |> column [ padding 8 ]
            ]
        ]


viewStar : Model -> EntityID -> Element Msg
viewStar model starId =
    case Logic.Component.get starId model.starForms of
        Nothing ->
            text "Your star is missing!"

        Just size ->
            Input.button
                []
                { label =
                    text <|
                        (\s -> s ++ " (" ++ String.fromInt starId ++ ")") <|
                            case size of
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
                , onPress = Just (SetFocus (FStar starId))
                }


viewPlanet : Model -> EntityID -> Element Msg
viewPlanet model planetId =
    case Logic.Component.get planetId model.celestialBodyForms of
        Nothing ->
            text "Your planet is missing!"

        Just planetType ->
            Input.button
                []
                { label =
                    text <|
                        case planetType of
                            Rocky ->
                                "Rocky"

                            Gas ->
                                "Gas"
                , onPress = Just (SetFocus (FPlanet planetId))
                }
