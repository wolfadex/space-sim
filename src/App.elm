module App exposing
    ( SpaceFocus(..)
    , Model(..)
    , Msg(..)
    , NewGameModel
    , NewGameMsg(..)
    , PlayingMsg
    , TickRate
    , World
    , init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , CivilizationReproductionRate
        , Knowledge(..)
        , Name
        , Orbit
        , StarSize(..)
        , Water
        )
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
import Random exposing (Generator, Seed)
import Random.Extra
import Random.List
import ScaledNumber exposing (ScaledNumber)
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared exposing (Flags)
import Time
import Ui.Button
import Ui.Text
import Ui.Theme
import View exposing (View)



---- INIT ----


type Model
    = NewGame NewGameModel
    | Playing World


type alias NewGameModel =
    { seed : Seed
    , civilizationNameSingular : String
    , civilizationNamePlural : String
    , hasUniquePluralName : Bool
    }


type alias World =
    { seed : Seed
    , spaceFocus : SpaceFocus
    , tickRate : TickRate

    ---- ECS stuff
    , ecsInternals : Logic.Entity.Extra.Internals

    -- CIV
    , civilizationPopulations : Logic.Component.Set (Dict EntityID ScaledNumber)
    , civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate
    , civilizationHappiness : Logic.Component.Set Float
    , civilizationKnowledge : Logic.Component.Set (AnySet String Knowledge)
    , named : Logic.Component.Set Name

    -- Other
    , planetTypes : Logic.Component.Set CelestialBodyForm
    , starForms : Logic.Component.Set StarSize
    , orbits : Logic.Component.Set Orbit
    , waterContent : Logic.Component.Set Water
    , planetSize : Logic.Component.Set Float
    , parents : Logic.Component.Set EntityID
    , children : Logic.Component.Set (Set EntityID)

    ---- Book keeping entities by ID
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , playerCiv : EntityID
    , civilizations : Set EntityID
    , availableCivilizationNames : List Name
    }


type SpaceFocus
    = FGalaxy
    | FSolarSystem EntityID
    | FStar EntityID
    | FPlanet EntityID


type TickRate
    = Paused
    | Normal
    | Fast
    | ExtraFast
    | HalfSpeed


emptyWorld : World
emptyWorld =
    { seed = Random.initialSeed 0
    , spaceFocus = FGalaxy
    , tickRate = Normal
    , ecsInternals = Logic.Entity.Extra.initInternals
    , named = Logic.Component.empty
    , civilizationReproductionRates = Logic.Component.empty
    , planetTypes = Logic.Component.empty
    , starForms = Logic.Component.empty
    , parents = Logic.Component.empty
    , children = Logic.Component.empty
    , orbits = Logic.Component.empty
    , waterContent = Logic.Component.empty
    , planetSize = Logic.Component.empty
    , civilizationPopulations = Logic.Component.empty
    , civilizationHappiness = Logic.Component.empty
    , civilizationKnowledge = Logic.Component.empty

    --
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , playerCiv = -1
    , civilizations = Set.empty
    , availableCivilizationNames = allCivilizationNames
    }


allCivilizationNames : List Name
allCivilizationNames =
    [ { singular = "Morlock"
      , plural = Just "Morlocks"
      }
    , { singular = "Klingon"
      , plural = Nothing
      }
    , { singular = "Federation"
      , plural = Nothing
      }
    ]


init : Flags -> ( Model, Effect Msg )
init flags =
    ( NewGame
        { seed = Random.initialSeed flags.seed0
        , civilizationNameSingular = "Carl"
        , civilizationNamePlural = "Carls"
        , hasUniquePluralName = True
        }
    , Effect.none
    )



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NewGame _ ->
            Sub.none

        Playing world ->
            case world.tickRate of
                Paused ->
                    Sub.none

                _ ->
                    Time.every (tickRateToMs world.tickRate) (\_ -> PlayingMessage Tick)


tickRateToMs : TickRate -> Float
tickRateToMs tickRate =
    let
        baseTickTime : Float
        baseTickTime =
            -- 3 seconds
            3000
    in
    case tickRate of
        Paused ->
            -- Infinity
            1 / 0

        HalfSpeed ->
            baseTickTime * 2

        Normal ->
            baseTickTime

        Fast ->
            baseTickTime / 2

        ExtraFast ->
            baseTickTime / 4


type Msg
    = NewGameMessage NewGameMsg
    | PlayingMessage PlayingMsg


type NewGameMsg
    = SetNameSingular String
    | SetNamePlural String
    | ToggleNamePlural Bool
    | StartGame


type PlayingMsg
    = DeleteGalaxy
    | SetFocus SpaceFocus
    | Tick
    | SetTickRate TickRate


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model ) of
        ( NewGameMessage message, NewGame newGameModel ) ->
            newGameUpdate message newGameModel

        ( PlayingMessage message, Playing world ) ->
            playingUpdate message world

        _ ->
            ( model, Effect.none )


newGameUpdate : NewGameMsg -> NewGameModel -> ( Model, Effect Msg )
newGameUpdate msg model =
    case msg of
        SetNameSingular singular ->
            ( NewGame { model | civilizationNameSingular = singular }
            , Effect.none
            )

        SetNamePlural plural ->
            ( NewGame { model | civilizationNamePlural = plural }
            , Effect.none
            )

        ToggleNamePlural enabled ->
            ( NewGame { model | hasUniquePluralName = enabled }
            , Effect.none
            )

        StartGame ->
            let
                ( generatedWorld, seed ) =
                    Random.step (generateGalaxy emptyWorld) model.seed

                viableStartingPlanets : List ( EntityID, Orbit )
                viableStartingPlanets =
                    generatedWorld.planets
                        |> Set.toList
                        |> List.filterMap
                            (\planetId ->
                                Maybe.andThen
                                    (\( type_, orbit ) ->
                                        case type_ of
                                            Rocky ->
                                                Just ( planetId, orbit )

                                            Gas ->
                                                Nothing
                                    )
                                    (Logic.Component.get2 planetId generatedWorld.planetTypes generatedWorld.orbits)
                            )
                        |> List.sortWith planetOrbitPreference

                ( shuffledPlanets, finalSeed ) =
                    Random.step (Random.List.shuffle viableStartingPlanets) seed

                ( playerCiv, worldWithPlayerCiv ) =
                    Logic.Entity.Extra.create generatedWorld
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , List.head shuffledPlanets
                                |> Maybe.map (\( planetId, _ ) -> Dict.singleton planetId (ScaledNumber.millions 100))
                                |> Maybe.withDefault Dict.empty
                            )
                        |> Logic.Entity.with
                            ( Game.Components.namedSpec
                            , { singular = model.civilizationNameSingular
                              , plural =
                                    if model.hasUniquePluralName then
                                        Just model.civilizationNameSingular

                                    else
                                        Nothing
                              }
                            )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, 1.1 )
                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, 1.0 )
                        |> Logic.Entity.with
                            ( Game.Components.knowledgeSpec
                            , Set.Any.fromList
                                Game.Components.knowledgeToString
                                [ LandTravel, WaterSurfaceTravel ]
                            )
            in
            ( Playing
                { worldWithPlayerCiv
                    | playerCiv = playerCiv
                    , seed = finalSeed
                    , civilizations = Set.insert playerCiv worldWithPlayerCiv.civilizations
                }
            , Effect.none
            )


planetOrbitPreference : ( EntityID, Orbit ) -> ( EntityID, Orbit ) -> Order
planetOrbitPreference ( _, orbitA ) ( _, orbitB ) =
    if orbitA == orbitB then
        EQ

    else if orbitA == 3 then
        GT

    else if orbitB == 3 then
        LT

    else if orbitA == 4 then
        GT

    else if orbitB == 4 then
        LT

    else if orbitA == 2 then
        GT

    else if orbitB == 2 then
        LT

    else if orbitA == 5 then
        GT

    else if orbitB == 5 then
        LT

    else
        EQ


playingUpdate : PlayingMsg -> World -> ( Model, Effect Msg )
playingUpdate msg world =
    case msg of
        SetTickRate tickRate ->
            ( Playing { world | tickRate = tickRate }, Effect.none )

        DeleteGalaxy ->
            ( NewGame
                { seed = world.seed
                , civilizationNameSingular = ""
                , civilizationNamePlural = ""
                , hasUniquePluralName = False
                }
            , Effect.none
            )

        SetFocus focus ->
            ( Playing { world | spaceFocus = focus }, Effect.none )

        Tick ->
            ( Playing
                (world
                    |> reproductionAndHappinessSystem
                        Game.Components.civilizationReproductionRateSpec
                        Game.Components.civilizationHappinessSpec
                    |> birthSystem
                        Game.Components.civilizationReproductionRateSpec
                        Game.Components.civilizationPopulationSpec
                    |> discoverySystem Game.Components.knowledgeSpec
                )
            , Effect.none
            )


discoverySystem : Spec (AnySet String Knowledge) World -> World -> World
discoverySystem knowledge world =
    let
        ( _, updatedKnowledge, seed ) =
            Array.foldl possiblyGainKnowledge
                ( 0, Logic.Component.empty, world.seed )
                (knowledge.get world)
    in
    knowledge.set updatedKnowledge { world | seed = seed }


possiblyGainKnowledge : Maybe (AnySet String Knowledge) -> ( Int, Array (Maybe (AnySet String Knowledge)), Seed ) -> ( Int, Array (Maybe (AnySet String Knowledge)), Seed )
possiblyGainKnowledge maybeCivKnowledge ( index, allCivsKnowledge, seed ) =
    case maybeCivKnowledge of
        Nothing ->
            ( index + 1, Array.set index Nothing allCivsKnowledge, seed )

        Just civKnowledge ->
            let
                ( updatedCivKnowledge, newSeed ) =
                    gainRandomKnowledge
                        civKnowledge
                        index
                        allCivsKnowledge
                        maybeCivKnowledge
                        seed
            in
            ( index + 1, updatedCivKnowledge, newSeed )


gainRandomKnowledge : AnySet String Knowledge -> Int -> Array (Maybe (AnySet String Knowledge)) -> Maybe (AnySet String Knowledge) -> Seed -> ( Array (Maybe (AnySet String Knowledge)), Seed )
gainRandomKnowledge civKnowledge index allCivsKnowledge maybeCivKnowledge seed =
    Random.step
        (Random.map
            (\gainsKnowledge ->
                if gainsKnowledge then
                    let
                        knows : Knowledge -> Bool
                        knows k =
                            Set.Any.member k civKnowledge

                        doesntKnow : Knowledge -> Bool
                        doesntKnow k =
                            not (Set.Any.member k civKnowledge)

                        giveKnowledge : Knowledge -> Array (Maybe (AnySet String Knowledge))
                        giveKnowledge learns =
                            Array.set index (Just (Set.Any.insert learns civKnowledge)) allCivsKnowledge
                    in
                    if knows UnderwaterTravel && doesntKnow WaterSurfaceTravel then
                        giveKnowledge WaterSurfaceTravel

                    else if (knows WaterSurfaceTravel || knows LandTravel) && doesntKnow Flight then
                        giveKnowledge Flight

                    else if knows Flight && doesntKnow PlanetarySpaceTravel then
                        giveKnowledge PlanetarySpaceTravel

                    else if knows PlanetarySpaceTravel && doesntKnow InterplanetarySpaceTravel then
                        giveKnowledge InterplanetarySpaceTravel

                    else if knows InterplanetarySpaceTravel && doesntKnow UnderwaterTravel then
                        giveKnowledge UnderwaterTravel

                    else if knows InterplanetarySpaceTravel && doesntKnow FTLSpaceTravel then
                        giveKnowledge FTLSpaceTravel

                    else
                        Array.set index maybeCivKnowledge allCivsKnowledge

                else
                    Array.set index maybeCivKnowledge allCivsKnowledge
            )
            (Random.Extra.oneIn 100)
        )
        seed


birthSystem : Spec CivilizationReproductionRate world -> Spec (Dict EntityID ScaledNumber) world -> System world
birthSystem =
    Logic.System.step2
        (\( reproductionRate, _ ) ( populationSizes, setPopulationSize ) ->
            setPopulationSize
                (Dict.map
                    (\_ populationSize ->
                        ScaledNumber.scaleBy reproductionRate populationSize
                    )
                    populationSizes
                )
        )


reproductionAndHappinessSystem : Spec CivilizationReproductionRate world -> Spec Float world -> System world
reproductionAndHappinessSystem =
    Logic.System.step2
        (\( reproductionRate, setReproductionRate ) ( happiness, setHappiness ) ->
            if reproductionRate > 1.1 && happiness > 1.1 then
                setHappiness (happiness - 0.2)

            else if reproductionRate >= 1.1 && happiness <= 1.1 then
                setReproductionRate (reproductionRate - 0.2)

            else if reproductionRate < 0.9 && happiness < 1.5 then
                setHappiness (happiness + 0.1)

            else
                setReproductionRate (reproductionRate + 0.1)
        )


generateGalaxy : World -> Generator World
generateGalaxy model =
    generateManyEntities 10 20 model generateSolarSystem
        |> Random.map Tuple.second


generateSolarSystem : ( EntityID, World ) -> Generator ( EntityID, World )
generateSolarSystem ( solarSystemId, world ) =
    generateManyEntities 1 3 world (generateStar solarSystemId)
        |> Random.andThen
            (\( starIds, starWorld ) ->
                Random.map
                    (\( planetIds, finalWorld ) ->
                        ( solarSystemId
                        , { finalWorld | solarSystems = Set.insert solarSystemId finalWorld.solarSystems }
                        )
                            |> Logic.Entity.with ( Game.Components.childrenSpec, Set.union planetIds starIds )
                    )
                    (generateManyEntities 1 12 starWorld (generatePlanet solarSystemId))
            )


generateStar : EntityID -> ( EntityID, World ) -> Generator ( EntityID, World )
generateStar solarSystemId ( starId, world ) =
    Random.map
        (\size ->
            ( starId, world )
                |> Logic.Entity.with ( Game.Components.starFormSpec, size )
                |> Logic.Entity.with ( Game.Components.parentSpec, solarSystemId )
                |> Tuple.mapSecond (\w -> { w | stars = Set.insert starId w.stars })
        )
        (Random.uniform Yellow
            [ RedGiant
            , BlueGiant
            , WhiteDwarf
            , BlackDwarf
            ]
        )


generatePlanet : EntityID -> ( EntityID, World ) -> Generator ( EntityID, World )
generatePlanet solarSystemId ( planetId, world ) =
    Random.map2 Tuple.pair
        (Random.uniform Rocky [ Gas ])
        generatePlanetWaterPercent
        |> Random.andThen
            (\( planetType, waterPercent ) ->
                Random.map3
                    (\orbit size updatedWorld ->
                        ( planetId, updatedWorld )
                            |> Logic.Entity.with ( Game.Components.planetTypeSpec, planetType )
                            |> Logic.Entity.with ( Game.Components.orbitSpec, orbit )
                            |> Logic.Entity.with ( Game.Components.waterSpec, waterPercent )
                            |> Logic.Entity.with ( Game.Components.planetSizeSpec, size )
                            |> Logic.Entity.with ( Game.Components.parentSpec, solarSystemId )
                            |> Tuple.mapSecond (\w -> { w | planets = Set.insert planetId w.planets })
                    )
                    (case planetType of
                        Rocky ->
                            Random.int 0 7

                        Gas ->
                            Random.int 5 12
                    )
                    (generatePlanetRadius planetType)
                    (attemptToGenerateCivilization planetType waterPercent planetId world)
            )


attemptToGenerateCivilization : CelestialBodyForm -> Float -> EntityID -> World -> Generator World
attemptToGenerateCivilization planetType waterPercent planetId world =
    if planetType == Rocky then
        Random.Extra.oneIn 10
            |> Random.andThen
                (\shouldCreateCiv ->
                    if shouldCreateCiv then
                        generateCivilizationName world
                            |> Random.andThen
                                (\( maybeName, worldWithFewerNames ) ->
                                    case maybeName of
                                        Nothing ->
                                            Random.constant worldWithFewerNames

                                        Just name ->
                                            generateCivilization waterPercent worldWithFewerNames planetId name
                                )

                    else
                        Random.constant world
                )

    else
        Random.constant world


generateCivilization : Float -> World -> EntityID -> Name -> Generator World
generateCivilization waterPercent worldWithFewerNames planetId name =
    Random.map4
        (\initialPopulationSize reproductionRate initialHappiness baseKnowledge ->
            let
                ( civId, worldWithNewCiv ) =
                    Logic.Entity.Extra.create worldWithFewerNames
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , Dict.singleton planetId (ScaledNumber.millions initialPopulationSize)
                            )
                        |> Logic.Entity.with ( Game.Components.namedSpec, name )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, reproductionRate )
                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, initialHappiness )
                        |> Logic.Entity.with
                            ( Game.Components.knowledgeSpec
                            , Set.Any.fromList
                                Game.Components.knowledgeToString
                                baseKnowledge
                            )
            in
            { worldWithNewCiv | civilizations = Set.insert civId worldWithNewCiv.civilizations }
        )
        (Random.float 50 150)
        (Random.float 0.8 1.5)
        (Random.float 0.5 1.5)
        (Random.weighted ( 1.0 - waterPercent, [ LandTravel ] )
            [ ( waterPercent, [ WaterSurfaceTravel ] )
            , if waterPercent > 0.9 then
                ( 1.0, [ UnderwaterTravel ] )

              else
                ( 0, [] )
            ]
        )


generateCivilizationName : World -> Generator ( Maybe Name, World )
generateCivilizationName world =
    Random.List.choose world.availableCivilizationNames
        |> Random.map
            (\( chosenName, remainingNames ) ->
                ( chosenName, { world | availableCivilizationNames = remainingNames } )
            )


{-| Generate the amount of water on a planet. For a Gas planet this would be water vapor.
-}
generatePlanetWaterPercent : Generator Water
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
generateManyEntities : Int -> Int -> World -> (( EntityID, World ) -> Generator ( EntityID, World )) -> Generator ( Set EntityID, World )
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


generateEntity : World -> (( EntityID, World ) -> Generator ( EntityID, World )) -> Generator ( EntityID, World )
generateEntity world fn =
    fn (Logic.Entity.Extra.create world)



---- VIEW ----


view : Model -> View Msg
view model =
    { title = "Hello Space!"
    , body =
        case model of
            NewGame newGameModel ->
                map NewGameMessage (viewNewGame newGameModel)

            Playing world ->
                map PlayingMessage (viewPlaying world)
    }



-- New Game View


viewNewGame : NewGameModel -> Element NewGameMsg
viewNewGame model =
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
                    , id = "singular-name"
                    , text = model.civilizationNameSingular
                    , label = Input.labelLeft [ width fill ] (text "Civilization Name Singular:")
                    }
                , Ui.Text.default
                    []
                    { onChange = SetNamePlural
                    , id = "plural-name"
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
                    , text " people begin to question the morality of continuing the war."
                    ]
                ]
            ]
        ]



-- Playing View


viewPlaying : World -> Element PlayingMsg
viewPlaying world =
    column
        [ width fill, height fill ]
        [ row
            [ padding 16, spacing 16 ]
            [ text "Game Speed:"
            , Ui.Button.toggle
                { label = text "||"
                , onPress = Just (SetTickRate Paused)
                , enabled = world.tickRate == Paused
                }
            , Ui.Button.toggle
                { label = text "|>"
                , onPress = Just (SetTickRate HalfSpeed)
                , enabled = world.tickRate == HalfSpeed
                }
            , Ui.Button.toggle
                { label = text ">"
                , onPress = Just (SetTickRate Normal)
                , enabled = world.tickRate == Normal
                }
            , Ui.Button.toggle
                { label = text ">>"
                , onPress = Just (SetTickRate Fast)
                , enabled = world.tickRate == Fast
                }
            , Ui.Button.toggle
                { label = text ">>>"
                , onPress = Just (SetTickRate ExtraFast)
                , enabled = world.tickRate == ExtraFast
                }
            , Ui.Button.default
                { label = text "Delete"
                , onPress = Just DeleteGalaxy
                }
            ]
        , row
            [ width fill
            , height fill
            , padding 16
            , spacing 8
            ]
            [ el
                [ alignTop
                , height fill
                , Border.solid
                , Border.width 1
                ]
                (case world.spaceFocus of
                    FGalaxy ->
                        viewGalaxy world

                    FSolarSystem id ->
                        if Set.member id world.solarSystems then
                            viewSlice (viewSolarSystem world id)

                        else
                            text "Missing solar system"

                    FStar starId ->
                        if Set.member starId world.stars then
                            viewSlice (viewBody world viewStar starId)

                        else
                            text "Missing star"

                    FPlanet planetId ->
                        if Set.member planetId world.planets then
                            viewSlice (viewBody world viewPlanetDetailed planetId)

                        else
                            text "Missing planet"
                )
            , viewCivilizations world
            ]
        ]


viewSlice : Element PlayingMsg -> Element PlayingMsg
viewSlice slice =
    column
        [ height fill
        ]
        [ Input.button
            []
            { label = text "View Galaxy"
            , onPress = Just (SetFocus FGalaxy)
            }
        , slice
        ]


viewBody : World -> (World -> EntityID -> Element PlayingMsg) -> EntityID -> Element PlayingMsg
viewBody model bodyFn id =
    column
        [ spacing 8, height fill ]
        [ Input.button
            []
            { label = text "View System"
            , onPress =
                Maybe.map (FSolarSystem >> SetFocus)
                    (Logic.Component.get id model.parents)
            }
        , bodyFn model id
        ]


viewGalaxy : World -> Element PlayingMsg
viewGalaxy model =
    Set.toList model.solarSystems
        |> List.map (viewSolarSystem model)
        |> column
            [ height fill
            , width fill
            ]


viewSolarSystem : World -> EntityID -> Element PlayingMsg
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
                |> List.map (Tuple.first >> viewPlanetSimple model)
                |> column [ padding 8 ]
            ]
        ]


viewStar : World -> EntityID -> Element PlayingMsg
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


viewPlanetSimple : World -> EntityID -> Element PlayingMsg
viewPlanetSimple world planetId =
    case Logic.Component.get planetId world.planetTypes of
        Nothing ->
            text "Your planet is missing!"

        Just planetType ->
            Input.button
                []
                { label =
                    text <|
                        case planetType of
                            Rocky ->
                                "Rocky: P_" ++ String.fromInt planetId

                            Gas ->
                                "Gas: P_" ++ String.fromInt planetId
                , onPress = Just (SetFocus (FPlanet planetId))
                }


viewPlanetDetailed : World -> EntityID -> Element PlayingMsg
viewPlanetDetailed world planetId =
    case Logic.Component.get planetId world.planetTypes of
        Nothing ->
            text "Your planet is missing!"

        Just planetType ->
            let
                civsOnPlanet : List ( EntityID, Name )
                civsOnPlanet =
                    Set.toList world.civilizations
                        |> List.filterMap
                            (\civId ->
                                Logic.Component.get civId world.civilizationPopulations
                                    |> Maybe.andThen
                                        (\dictPlanetPopulatiopns ->
                                            if Dict.member planetId dictPlanetPopulatiopns then
                                                Maybe.map (Tuple.pair civId)
                                                    (Logic.Component.get civId world.named)

                                            else
                                                Nothing
                                        )
                            )
            in
            column
                [ spacing 8 ]
                [ text <|
                    case planetType of
                        Rocky ->
                            "Rocky: P_" ++ String.fromInt planetId

                        Gas ->
                            "Gas: P_" ++ String.fromInt planetId
                , text "Civs on Planet:"
                , case civsOnPlanet of
                    [] ->
                        text "None"

                    civs ->
                        List.map
                            (\( civId, name ) ->
                                Ui.Button.default
                                    { label = text name.singular
                                    , onPress = Nothing
                                    }
                            )
                            civs
                            |> column [ spacing 4 ]
                ]


viewCivilizations : World -> Element PlayingMsg
viewCivilizations world =
    world.civilizations
        |> Set.toList
        |> List.map (viewCivilization world)
        |> column [ spacing 8, alignTop ]


viewCivilization : World -> EntityID -> Element PlayingMsg
viewCivilization world civId =
    column
        [ padding 16
        , width fill
        , spacing 16
        , Border.solid
        , Border.width 2
        , Border.color Ui.Theme.darkGray
        , Background.color <|
            if civId == world.playerCiv then
                Ui.Theme.green

            else
                Ui.Theme.nearlyWhite
        ]
        (case getCivilizationDetails world civId of
            Nothing ->
                [ text "Civ is missing" ]

            Just details ->
                let
                    totalPopulationSize : ScaledNumber
                    totalPopulationSize =
                        details.occupiedPlanets
                            |> Dict.toList
                            |> List.foldl (\( _, planetPupulationCount ) -> ScaledNumber.sum planetPupulationCount) (ScaledNumber.millions 0)
                in
                [ text ("The " ++ Maybe.withDefault details.name.singular details.name.plural ++ " have " ++ ScaledNumber.toString totalPopulationSize ++ " citizens.")
                , text ("Happiness " ++ happinessToString details.happiness)
                , text "They occuy planets:"
                , details.occupiedPlanets
                    |> Dict.toList
                    |> List.map
                        (\( planetId, populationCount ) ->
                            row
                                [ spacing 8 ]
                                [ Ui.Button.default
                                    { label = text ("P_" ++ String.fromInt planetId)
                                    , onPress = Just (SetFocus (FPlanet planetId))
                                    }
                                , paragraph [ padding 8 ]
                                    [ text ("population: " ++ ScaledNumber.toString populationCount)
                                    ]
                                ]
                        )
                    |> column []
                ]
        )


happinessToString : Float -> String
happinessToString happiness =
    if happiness > 1.2 then
        ":D"

    else if happiness > 1.0 then
        ":)"

    else if happiness == 1.0 then
        ":|"

    else if happiness < 0.8 then
        "D:"

    else
        "):"


getCivilizationDetails :
    World
    -> EntityID
    ->
        Maybe
            { name : Name
            , occupiedPlanets : Dict EntityID ScaledNumber
            , happiness : Float
            }
getCivilizationDetails world civId =
    Maybe.map2
        (\name happiness ->
            { name = name
            , occupiedPlanets =
                Logic.Component.get civId world.civilizationPopulations
                    |> Maybe.withDefault Dict.empty
            , happiness = happiness
            }
        )
        (Logic.Component.get civId world.named)
        (Logic.Component.get civId world.civilizationHappiness)
