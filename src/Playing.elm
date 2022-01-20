module Playing exposing
    ( Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Data.Names exposing (CivilizationName)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Galaxy2d exposing (viewSolarSystem)
import Galaxy3d
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , CivilizationFocus(..)
        , Happiness
        , Knowledge(..)
        , LightYear
        , Log
        , Mortality
        , Orbit
        , Reproduction
        , SpaceFocus(..)
        , StarDate
        , StarSize(..)
        , TickRate(..)
        , ViewStyle(..)
        , Water
        , World
        , emptyWorld
        )
import Length exposing (Length, Meters)
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Quantity
import Random exposing (Generator, Seed)
import Random.Extra
import Random.List
import Rate exposing (Rate)
import Round
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared exposing (Effect)
import SubCmd exposing (SubCmd)
import Time
import Ui.Button
import Ui.Theme
import View exposing (View)



---- INIT ----


init : { name : CivilizationName, homePlanetName : String, seed : Seed } -> ( World, SubCmd Msg Effect )
init flags =
    let
        -- Filter out a civilization name if the player's chosen name matches
        worldWithPlayerDataFilteredOut : World
        worldWithPlayerDataFilteredOut =
            { emptyWorld
                | availableCivilizationNames =
                    List.filter (\name -> String.toLower name.singular /= String.toLower flags.name.singular)
                        emptyWorld.availableCivilizationNames
            }

        ( generatedWorld, seed ) =
            Random.step (generateGalaxy worldWithPlayerDataFilteredOut) flags.seed

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

        ( inhabitedPlanets, finalSeed ) =
            Random.step (Random.List.shuffle viableStartingPlanets) seed
                |> Tuple.mapFirst
                    (List.head
                        >> Maybe.map (\( planetId, _ ) -> Dict.singleton planetId (Population.millions 7))
                        >> Maybe.withDefault Dict.empty
                    )

        ( playerCiv, worldWithPlayerCiv ) =
            Logic.Entity.Extra.create generatedWorld
                |> Logic.Entity.with ( Game.Components.civilizationPopulationSpec, inhabitedPlanets )
                |> Logic.Entity.with ( Game.Components.namedSpec, flags.name )
                |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, Rate.fromFloat 0.3 )
                |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, Rate.fromFloat 0.1 )
                |> Logic.Entity.with
                    ( Game.Components.civilizationHappinessSpec
                    , Dict.map (\_ _ -> Percent.fromFloat 100.0) inhabitedPlanets
                    )
                |> Logic.Entity.with ( Game.Components.knowledgeSpec, Set.Any.empty )
    in
    ( { worldWithPlayerCiv
        | playerCiv = playerCiv
        , seed = finalSeed
        , civilizations = Set.insert playerCiv worldWithPlayerCiv.civilizations
      }
    , SubCmd.none
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



---- UPDATE ----


subscriptions : World -> Sub Msg
subscriptions world =
    case world.tickRate of
        Paused ->
            Sub.none

        _ ->
            Time.every (tickRateToMs world.tickRate) (\_ -> Tick)


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
            baseTickTime / 4

        ExtraFast ->
            baseTickTime / 8


type Msg
    = DeleteGalaxy
    | SetSpaceFocus SpaceFocus
    | SetCivilizationFocus CivilizationFocus
    | Tick
    | SetTickRate TickRate
    | GotViewStyle ViewStyle


update : Msg -> World -> ( World, SubCmd Msg Effect )
update msg world =
    case msg of
        SetTickRate tickRate ->
            ( { world | tickRate = tickRate }, SubCmd.none )

        DeleteGalaxy ->
            ( world
            , SubCmd.effect (Shared.DeleteGame world.seed)
            )

        SetSpaceFocus focus ->
            ( { world | spaceFocus = focus }, SubCmd.none )

        SetCivilizationFocus focus ->
            ( { world | civilizationFocus = focus }, SubCmd.none )

        GotViewStyle viewStyle ->
            ( { world | viewStyle = viewStyle }, SubCmd.none )

        Tick ->
            ( { world | starDate = world.starDate + 1 }
                |> happinessSystem
                    Game.Components.civilizationReproductionRateSpec
                    Game.Components.civilizationMortalityRateSpec
                    Game.Components.civilizationHappinessSpec
                |> birthAndDeathSystem
                    Game.Components.civilizationReproductionRateSpec
                    Game.Components.civilizationMortalityRateSpec
                    Game.Components.civilizationPopulationSpec
                |> discoverySystem Game.Components.knowledgeSpec
                |> civilUnrestSystem
            , SubCmd.none
            )


civilUnrestSystem : World -> World
civilUnrestSystem world =
    let
        revoltingCivs : List ( EntityID, EntityID )
        revoltingCivs =
            world.civilizations
                |> Set.toList
                |> List.concatMap
                    (\civId ->
                        Maybe.map
                            (Dict.foldl
                                (\planetId planetHappiness planetsRevolting ->
                                    if Quantity.lessThan (Percent.fromFloat 15.0) planetHappiness then
                                        ( civId, planetId ) :: planetsRevolting

                                    else
                                        planetsRevolting
                                )
                                []
                            )
                            (Logic.Component.get civId world.civilizationHappiness)
                            |> Maybe.withDefault []
                    )

        ( worldWithNewCivs, seed ) =
            Random.step (generateRevoltingCivs revoltingCivs world) world.seed
    in
    { worldWithNewCivs | seed = seed }


generateRevoltingCivs : List ( EntityID, EntityID ) -> World -> Generator World
generateRevoltingCivs revoltingCivs world =
    List.foldl
        (\( civId, planetId ) updatedWorld ->
            case getCivilizationDetails world civId of
                Nothing ->
                    updatedWorld

                Just details ->
                    Random.andThen
                        (\w ->
                            generateRevoltingCivilization w civId planetId details
                        )
                        updatedWorld
        )
        (Random.constant world)
        revoltingCivs


{-|

  - Assign the planet to the new Civ, aka the revolting populace
  - The old civ loses a percent of their knowledge (penalty for having revolts)
  - The new civ loses a larger percent of that knowledge (penalty for revolting)

-}
generateRevoltingCivilization : World -> EntityID -> EntityID -> CivilizationDetails -> Generator World
generateRevoltingCivilization world oldCivId planetId civDetails =
    Random.map3
        (\initialHappiness knowledge oldCivNewKnowledge ->
            let
                ( civId, worldWithNewCiv ) =
                    Logic.Entity.Extra.create world
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , Dict.singleton planetId
                                (Maybe.withDefault (Population.millions 3)
                                    (Dict.get planetId civDetails.occupiedPlanets)
                                )
                            )
                        |> Logic.Entity.with
                            ( Game.Components.namedSpec
                            , { singular = "Renegade " ++ civDetails.name.singular
                              , possessive = Maybe.map ((++) "Renegade ") civDetails.name.possessive
                              , many = Maybe.map ((++) "Renegade ") civDetails.name.many
                              }
                            )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, civDetails.reproductionRate )
                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, civDetails.mortalityRate )
                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
                        |> Logic.Entity.with ( Game.Components.knowledgeSpec, knowledge )
            in
            { worldWithNewCiv
                | civilizations = Set.insert civId worldWithNewCiv.civilizations
                , civilizationKnowledge =
                    Logic.Component.update oldCivId
                        (\_ -> oldCivNewKnowledge)
                        worldWithNewCiv.civilizationKnowledge
            }
        )
        (Percent.random 95.0 100.0)
        (Random.andThen
            (\percent -> dropRandom Game.Components.knowledgeComparableConfig percent civDetails.knowledge)
            (Percent.random 5.0 25.0)
        )
        (Random.andThen
            (\percent -> dropRandom Game.Components.knowledgeComparableConfig percent civDetails.knowledge)
            (Percent.random 1.0 5.0)
        )


dropRandom : { r | fromComparable : comparable -> b, toComparable : b -> comparable } -> Percent a -> AnySet comparable b -> Generator (AnySet comparable b)
dropRandom anySetConfig percent set =
    let
        setList : List b
        setList =
            Set.Any.toList anySetConfig set

        toDrop : Int
        toDrop =
            Quantity.multiplyBy 100.0 percent
                |> Quantity.divideBy (toFloat (List.length setList))
                |> Percent.toFloat
                |> floor
    in
    Random.map (List.drop toDrop >> Set.Any.fromList anySetConfig)
        (Random.List.shuffle setList)


discoverySystem : Spec (AnySet String Knowledge) World -> World -> World
discoverySystem knowledge world =
    let
        initialUpdatedKnowledge : Array (Maybe (AnySet String Knowledge))
        initialUpdatedKnowledge =
            Array.initialize
                (Array.length (knowledge.get world))
                (\_ -> Nothing)

        updates :
            { index : Int
            , updatedKnowledge : Array (Maybe (AnySet String Knowledge))
            , seed : Seed
            , civNames : Array (Maybe CivilizationName)
            , starDate : StarDate
            , logs : List Log
            }
        updates =
            Array.foldl possiblyGainKnowledge
                { index = 0
                , updatedKnowledge = initialUpdatedKnowledge
                , seed = world.seed
                , civNames = world.named
                , starDate = world.starDate
                , logs = []
                }
                (knowledge.get world)
    in
    knowledge.set updates.updatedKnowledge
        { world
            | seed = updates.seed
            , eventLog = updates.logs ++ world.eventLog
        }


possiblyGainKnowledge :
    Maybe (AnySet String Knowledge)
    ->
        { index : Int
        , updatedKnowledge : Array (Maybe (AnySet String Knowledge))
        , seed : Seed
        , civNames : Array (Maybe CivilizationName)
        , starDate : StarDate
        , logs : List Log
        }
    ->
        { index : Int
        , updatedKnowledge : Array (Maybe (AnySet String Knowledge))
        , seed : Seed
        , civNames : Array (Maybe CivilizationName)
        , starDate : StarDate
        , logs : List Log
        }
possiblyGainKnowledge maybeCivKnowledge ({ index, updatedKnowledge, seed, starDate, civNames } as updates) =
    case maybeCivKnowledge of
        Nothing ->
            { updates | index = index + 1, updatedKnowledge = Array.set index Nothing updatedKnowledge }

        Just civKnowledge ->
            let
                civName : CivilizationName
                civName =
                    case Array.get index civNames of
                        Just (Just name) ->
                            name

                        _ ->
                            { singular = "", many = Nothing, possessive = Nothing }

                ( ( updatedCivKnowledge, maybeLog ), newSeed ) =
                    gainRandomKnowledge
                        civKnowledge
                        index
                        updatedKnowledge
                        maybeCivKnowledge
                        seed
                        starDate
                        civName
            in
            { updates
                | index = index + 1
                , updatedKnowledge = updatedCivKnowledge
                , seed = newSeed
                , logs =
                    case maybeLog of
                        Nothing ->
                            updates.logs

                        Just log ->
                            log :: updates.logs
            }


gainRandomKnowledge :
    AnySet String Knowledge
    -> Int
    -> Array (Maybe (AnySet String Knowledge))
    -> Maybe (AnySet String Knowledge)
    -> Seed
    -> StarDate
    -> CivilizationName
    -> ( ( Array (Maybe (AnySet String Knowledge)), Maybe Log ), Seed )
gainRandomKnowledge civKnowledge index allCivsKnowledge maybeCivKnowledge seed starDate civName =
    Random.step
        (Random.map2
            (\gainsKnowledge personName ->
                if gainsKnowledge then
                    let
                        knows : Knowledge -> Bool
                        knows k =
                            Set.Any.member Game.Components.knowledgeComparableConfig k civKnowledge

                        doesntKnow : Knowledge -> Bool
                        doesntKnow k =
                            not (Set.Any.member Game.Components.knowledgeComparableConfig k civKnowledge)

                        giveKnowledge : Knowledge -> (String -> String) -> ( Array (Maybe (AnySet String Knowledge)), Maybe Log )
                        giveKnowledge learns description =
                            ( Array.set index (Just (Set.Any.insert Game.Components.knowledgeComparableConfig learns civKnowledge)) allCivsKnowledge
                            , Just
                                { time = starDate
                                , description = description (Data.Names.enhancedEventDescription civName personName)
                                , civilizationId = index
                                }
                            )
                    in
                    if doesntKnow BasicAgriculture then
                        giveKnowledge BasicAgriculture (\name -> "After much trial and error, eating the wrong foods, " ++ name ++ " manages to figure out rudimentary agriculture.")

                    else if doesntKnow BasicMetalWorking then
                        giveKnowledge BasicMetalWorking (\name -> "After many burnt appendages, the secrets of metal working were unlocked by " ++ name)

                    else if doesntKnow WaterSurfaceTravel then
                        giveKnowledge WaterSurfaceTravel (\name -> name ++ " takes a ride on a floating log, then claims to have invedted boating.")

                    else if knows UnderwaterTravel && doesntKnow WaterSurfaceTravel then
                        giveKnowledge WaterSurfaceTravel (\name -> name ++ " learns to build boats.")

                    else if (knows WaterSurfaceTravel || knows LandTravel) && doesntKnow Flight then
                        giveKnowledge Flight (\name -> name ++ " learns the art of flying.")

                    else if knows Flight && doesntKnow PlanetarySpaceTravel then
                        giveKnowledge PlanetarySpaceTravel (\name -> name ++ " takes a leap of faith into space.")

                    else if knows PlanetarySpaceTravel && doesntKnow InterplanetarySpaceTravel then
                        giveKnowledge InterplanetarySpaceTravel (\name -> name ++ " begins their solar voyage.")

                    else if knows InterplanetarySpaceTravel && doesntKnow UnderwaterTravel then
                        giveKnowledge UnderwaterTravel (\name -> name ++ " thinks it's a good idea to build underwater vessels.")

                    else if knows InterplanetarySpaceTravel && doesntKnow LandTravel then
                        giveKnowledge LandTravel (\name -> name ++ " thinks it's a good idea to put wheels on a boat.")

                    else if knows InterplanetarySpaceTravel && doesntKnow FTLSpaceTravel then
                        giveKnowledge FTLSpaceTravel (\name -> name ++ " makes the faster than light leap.")

                    else
                        ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )

                else
                    ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )
            )
            (Random.Extra.oneIn 100)
            Data.Names.randomPerson
        )
        seed


birthAndDeathSystem : Spec (Rate Reproduction) world -> Spec (Rate Mortality) world -> Spec (Dict EntityID Population) world -> System world
birthAndDeathSystem =
    Logic.System.step3
        (\( reproductionRate, _ ) ( mortalityRate, _ ) ( populationSizes, setPopulationSize ) ->
            setPopulationSize
                (Dict.map
                    (\_ populationSize ->
                        let
                            births : Population
                            births =
                                Population.multiplyBy (Rate.toFloat reproductionRate) populationSize

                            deaths : Population
                            deaths =
                                Population.multiplyBy (Rate.toFloat mortalityRate) populationSize
                        in
                        Population.plus (Population.difference populationSize deaths) births
                    )
                    populationSizes
                )
        )


happinessSystem : Spec (Rate Reproduction) world -> Spec (Rate Mortality) world -> Spec (Dict EntityID (Percent Happiness)) world -> System world
happinessSystem =
    Logic.System.step3
        (\( reproductionRate, setReproductionRate ) ( mortalityRate, setMortalityRate ) ( happinessPercent, setHappiness ) ->
            let
                averageHappiness : Percent Happiness
                averageHappiness =
                    averageCivilizationHappiness happinessPercent

                repro : Float
                repro =
                    Rate.toFloat reproductionRate

                happiness : Float
                happiness =
                    Percent.toFloat averageHappiness

                mortality : Float
                mortality =
                    Rate.toFloat mortalityRate

                -- Providing type annotations here causes a Haskell runtime error
                newHappiness =
                    (if mortality > repro then
                        -0.1

                     else if repro > mortality * 2 then
                        -0.1

                     else
                        0.1
                    )
                        |> Percent.fromFloat
                        |> (\percentChange -> Dict.map (\_ -> Quantity.plus percentChange) happinessPercent)
                        |> setHappiness

                -- Providing type annotations here causes a Haskell runtime error
                newReproductinRate =
                    (if happiness > 80.0 then
                        repro + 0.01

                     else
                        repro - 0.01
                    )
                        |> Rate.fromFloat
                        |> setReproductionRate

                -- Providing type annotations here causes a Haskell runtime error
                newMortalityRate =
                    (if happiness < 50.0 then
                        mortality - 0.01

                     else
                        mortality + 0.01
                    )
                        |> Rate.fromFloat
                        |> setMortalityRate
            in
            newHappiness >> newReproductinRate >> newMortalityRate
        )


generateGalaxy : World -> Generator World
generateGalaxy model =
    generateManyEntities 100 200 model generateSolarSystem
        |> Random.map Tuple.second


generateSolarSystem : ( EntityID, World ) -> Generator ( EntityID, World )
generateSolarSystem ( solarSystemId, world ) =
    generateManyEntities 1 3 world (generateStar solarSystemId)
        |> Random.andThen
            (\( starIds, starWorld ) ->
                Random.map2
                    (\( planetIds, finalWorld ) galacticPosition ->
                        ( solarSystemId
                        , { finalWorld | solarSystems = Set.insert solarSystemId finalWorld.solarSystems }
                        )
                            |> Logic.Entity.with ( Game.Components.childrenSpec, Set.union planetIds starIds )
                            |> Logic.Entity.with ( Game.Components.positionSpec, galacticPosition )
                    )
                    (generateManyEntities 1 12 starWorld (generatePlanet solarSystemId))
                    generateGalacticPosition
            )


generateGalacticPosition : Generator (Point3d Meters LightYear)
generateGalacticPosition =
    Random.map3
        (\randT randU1 randU2 ->
            let
                t : Float
                t =
                    2 * pi * randT

                u : Float
                u =
                    randU1 + randU2

                r : Float
                r =
                    if u > 1 then
                        2 - u

                    else
                        u

                x : Length
                x =
                    Length.lightYears ((r * cos t) * 5000)

                y : Length
                y =
                    Length.lightYears ((r * sin t) * 5000)
            in
            Point3d.fromMeters
                { x = Length.inMeters x
                , y = Length.inMeters y
                , z = 0
                }
        )
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)


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
                    (attemptToGenerateCivilization planetType planetId world)
            )


attemptToGenerateCivilization : CelestialBodyForm -> EntityID -> World -> Generator World
attemptToGenerateCivilization planetType planetId world =
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
                                            generateCivilization worldWithFewerNames planetId name
                                )

                    else
                        Random.constant world
                )

    else
        Random.constant world


generateCivilization : World -> EntityID -> CivilizationName -> Generator World
generateCivilization worldWithFewerNames planetId name =
    Random.map4
        (\initialPopulationSize reproductionRate mortalityRate initialHappiness ->
            let
                ( civId, worldWithNewCiv ) =
                    Logic.Entity.Extra.create worldWithFewerNames
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , Dict.singleton planetId (Population.millions initialPopulationSize)
                            )
                        |> Logic.Entity.with ( Game.Components.namedSpec, name )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, reproductionRate )
                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, mortalityRate )
                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
                        |> Logic.Entity.with ( Game.Components.knowledgeSpec, Set.Any.empty )
            in
            { worldWithNewCiv | civilizations = Set.insert civId worldWithNewCiv.civilizations }
        )
        (Random.float 3 10)
        (Rate.random 0.2 0.3)
        (Rate.random 0.1 0.2)
        (Percent.random 90.0 100.0)


generateCivilizationName : World -> Generator ( Maybe CivilizationName, World )
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


view : World -> View Msg
view world =
    { title = "Hello Space!"
    , body = viewPlaying world
    }


viewPlaying : World -> Element Msg
viewPlaying world =
    column
        [ width fill
        , height fill
        ]
        [ viewControls world
        , row
            [ width fill
            , height fill
            , scrollbarY
            , padding 16
            , spacing 8
            ]
            [ el
                [ alignTop
                , width fill
                , height fill
                , scrollbarY
                , Border.solid
                , Border.width 1
                ]
                (case world.spaceFocus of
                    FGalaxy ->
                        viewGalaxy world

                    FSolarSystem id ->
                        if Set.member id world.solarSystems then
                            viewSlice (viewSolarSystemDetailed world id)

                        else
                            text "Missing solar system"

                    FStar starId ->
                        if Set.member starId world.stars then
                            viewSlice (viewBody world viewStarDetailed starId)

                        else
                            text "Missing star"

                    FPlanet planetId ->
                        if Set.member planetId world.planets then
                            viewSlice (viewBody world viewPlanetDetailed planetId)

                        else
                            text "Missing planet"
                )
            , case world.civilizationFocus of
                FAll ->
                    viewCivilizations world

                FOne civId ->
                    viewCivilizationDetailed world civId
            ]
        ]


viewControls : World -> Element Msg
viewControls world =
    row
        [ padding 16
        , spacing 16
        ]
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
        , text ("Star Date: " ++ String.fromInt world.starDate)
        , Ui.Button.default <|
            case world.viewStyle of
                ThreeD ->
                    { label = text "View 2D Glaxy"
                    , onPress = Just (GotViewStyle TwoD)
                    }

                TwoD ->
                    { label = text "View 3D Glaxy"
                    , onPress = Just (GotViewStyle ThreeD)
                    }
        ]


viewSlice : Element Msg -> Element Msg
viewSlice slice =
    column
        [ height fill
        , padding 8
        ]
        [ Ui.Button.default
            { label = text "View Galaxy"
            , onPress = Just (SetSpaceFocus FGalaxy)
            }
        , slice
        ]


viewBody : World -> (World -> EntityID -> Element Msg) -> EntityID -> Element Msg
viewBody model bodyFn id =
    column
        [ spacing 8, height fill, padding 8 ]
        [ Ui.Button.default
            { label = text "View System"
            , onPress =
                Maybe.map (FSolarSystem >> SetSpaceFocus)
                    (Logic.Component.get id model.parents)
            }
        , bodyFn model id
        ]


viewGalaxy : World -> Element Msg
viewGalaxy world =
    case world.viewStyle of
        ThreeD ->
            Galaxy3d.viewGalaxy world (FSolarSystem >> SetSpaceFocus)

        TwoD ->
            Galaxy2d.viewGalaxy
                { onPressSolarSystem = FSolarSystem >> SetSpaceFocus
                , onPressCivilization = FOne >> SetCivilizationFocus
                }
                world


viewSolarSystemDetailed : World -> EntityID -> Element Msg
viewSolarSystemDetailed world solarSystemId =
    let
        ( stars, planets ) =
            Logic.Component.get solarSystemId world.children
                |> Maybe.map
                    (\children ->
                        ( Set.intersect children world.stars
                        , Set.intersect children world.planets
                        )
                    )
                |> Maybe.withDefault ( Set.empty, Set.empty )
    in
    case world.viewStyle of
        ThreeD ->
            Galaxy3d.viewSolarSystem
                { onPressStar = FStar >> SetSpaceFocus
                , onPressPlanet = FPlanet >> SetSpaceFocus
                , stars = stars
                , planets = planets
                }
                world

        TwoD ->
            viewSolarSystem
                { onPressPlanet = FPlanet >> SetSpaceFocus
                , onPressStar = FStar >> SetSpaceFocus
                , onPressCivilization = FOne >> SetCivilizationFocus
                }
                solarSystemId
                stars
                world
                planets


viewStarDetailed : World -> EntityID -> Element Msg
viewStarDetailed model starId =
    case Logic.Component.get starId model.starForms of
        Nothing ->
            text "Your star is missing!"

        Just size ->
            let
                sizeStr : String
                sizeStr =
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
            in
            column
                [ spacing 8 ]
                [ text ("Name: S_" ++ String.fromInt starId)
                , text ("Size: " ++ sizeStr)
                ]


viewPlanetDetailed : World -> EntityID -> Element Msg
viewPlanetDetailed world planetId =
    case Logic.Component.get planetId world.planetTypes of
        Nothing ->
            text "Your planet is missing!"

        Just planetType ->
            let
                civsOnPlanet : List ( EntityID, CivilizationName )
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
                [ text ("Name: P_" ++ String.fromInt planetId)
                , text
                    ("Terrain: "
                        ++ (case planetType of
                                Rocky ->
                                    "Rocky"

                                Gas ->
                                    "Gas"
                           )
                    )
                , text "Civs on Planet:"
                , case civsOnPlanet of
                    [] ->
                        text "None"

                    civs ->
                        List.map
                            (\( civId, name ) ->
                                Ui.Button.default
                                    { label = text name.singular
                                    , onPress = Just (SetCivilizationFocus (FOne civId))
                                    }
                            )
                            civs
                            |> column [ spacing 4 ]
                ]


viewCivilizations : World -> Element Msg
viewCivilizations world =
    world.civilizations
        |> Set.toList
        |> List.map (viewCivilizationSimple world)
        |> column
            [ spacing 8
            , alignTop
            , width fill
            , scrollbarY
            ]


viewCivilizationSimple : World -> EntityID -> Element Msg
viewCivilizationSimple world civId =
    case Logic.Component.get civId world.named of
        Nothing ->
            text "Civilization is missing"

        Just name ->
            row
                [ spacing 8
                , width fill
                , Background.color <|
                    if civId == world.playerCiv then
                        Ui.Theme.green

                    else
                        Ui.Theme.nearlyWhite
                ]
                [ Ui.Button.inspect
                    (Just (SetCivilizationFocus (FOne civId)))
                , text name.singular
                ]


viewCivilizationDetailed : World -> EntityID -> Element Msg
viewCivilizationDetailed world civId =
    column
        [ padding 16
        , width fill
        , height fill
        , scrollbarY
        , spacing 16
        , alignTop
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
                    totalPopulationSize : Population
                    totalPopulationSize =
                        details.occupiedPlanets
                            |> Dict.toList
                            |> List.foldl (\( _, planetPupulationCount ) -> Population.plus planetPupulationCount) (Population.millions 0)
                in
                [ Ui.Button.default
                    { label = text "Back"
                    , onPress = Just (SetCivilizationFocus FAll)
                    }
                , text ("The " ++ Maybe.withDefault details.name.singular details.name.possessive ++ " have " ++ populationToString totalPopulationSize ++ " citizens.")
                , text ("Happiness " ++ happinessToString (averageCivilizationHappiness details.happiness))
                , text "They occupy planets:"
                , details.occupiedPlanets
                    |> Dict.toList
                    |> List.map
                        (\( planetId, populationCount ) ->
                            row
                                [ spacing 8 ]
                                [ Ui.Button.default
                                    { label = text ("P_" ++ String.fromInt planetId)
                                    , onPress = Just (SetSpaceFocus (FPlanet planetId))
                                    }
                                , column [ spacing 4, width fill ]
                                    [ paragraph [] [ text ("Population: " ++ populationToString populationCount) ]
                                    , paragraph []
                                        [ case Dict.get planetId details.happiness of
                                            Just happiness ->
                                                text ("Happiness: " ++ happinessToString happiness)

                                            Nothing ->
                                                none
                                        ]
                                    ]
                                ]
                        )
                    |> column []
                , text "Logs"
                , details.logs
                    |> List.map viewLog
                    |> column [ spacing 4 ]
                ]
        )


averageCivilizationHappiness : Dict EntityID (Percent Happiness) -> Percent Happiness
averageCivilizationHappiness happiness =
    Dict.toList happiness
        |> List.foldl (\( _, happinessPerPlanet ) -> Quantity.plus happinessPerPlanet) Percent.zero
        |> Quantity.divideBy (toFloat (Dict.size happiness))


populationToString : Population -> String
populationToString population =
    let
        postFix : String -> (Population -> Float) -> String
        postFix str fn =
            Round.round 3 (fn population) ++ str
    in
    if Quantity.lessThan Population.billion population then
        postFix " million" Population.inMillions

    else if Quantity.lessThan Population.trillion population then
        postFix " billion" Population.inBillions

    else
        postFix " trillion" Population.inTrillions


viewLog : Log -> Element Msg
viewLog log =
    column
        [ Border.solid
        , Border.width 1
        , padding 4
        ]
        [ text ("Star Date: " ++ String.fromInt log.time)
        , paragraph [] [ text log.description ]
        ]


happinessToString : Percent Happiness -> String
happinessToString happiness =
    let
        hap : Float
        hap =
            Percent.toFloat happiness
    in
    if hap > 1.2 then
        ":D"

    else if hap > 1.0 then
        ":)"

    else if hap == 1.0 then
        ":|"

    else if hap < 0.8 then
        "D:"

    else
        "):"


type alias CivilizationDetails =
    { occupiedPlanets : Dict EntityID Population
    , reproductionRate : Rate Reproduction
    , mortalityRate : Rate Mortality
    , knowledge : AnySet String Knowledge
    , logs : List Log
    , name : CivilizationName
    , happiness : Dict EntityID (Percent Happiness)
    }


getCivilizationDetails : World -> EntityID -> Maybe CivilizationDetails
getCivilizationDetails world civId =
    Maybe.map4
        (\reproductionRate mortalityRate name happiness ->
            { occupiedPlanets =
                Logic.Component.get civId world.civilizationPopulations
                    |> Maybe.withDefault Dict.empty
            , reproductionRate = reproductionRate
            , mortalityRate = mortalityRate
            , knowledge =
                Logic.Component.get civId world.civilizationKnowledge
                    |> Maybe.withDefault Set.Any.empty
            , logs = List.filter (.civilizationId >> (==) civId) world.eventLog
            , name = name
            , happiness = happiness
            }
        )
        (Logic.Component.get civId world.civilizationReproductionRates)
        (Logic.Component.get civId world.civilizationMortalityRates)
        (Logic.Component.get civId world.named)
        (Logic.Component.get civId world.civilizationHappiness)
