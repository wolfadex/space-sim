module Playing exposing
    ( init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Browser.Events
import Data.Civilization
import Data.EarthYear
import Data.Knowledge exposing (Knowledge(..))
import Data.Name exposing (Name)
import Data.Orbit exposing (Orbit)
import Data.Star
import Data.Structure
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Galaxy2d
import Galaxy3d
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , CivilizationFocus(..)
        , Happiness
        , LightYear
        , Log
        , Mortality
        , PlayingMsg(..)
        , Reproduction
        , SolarSystem(..)
        , SpaceFocus(..)
        , TickRate(..)
        , ViewStyle(..)
        , Visible(..)
        , World
        , emptyWorld
        )
import Json.Decode
import Length exposing (Meters)
import List.Nonempty
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
import Markov.String
import Percent exposing (Percent)
import Point3d exposing (Point3d)
import Population exposing (Population)
import Process
import Quantity
import Random exposing (Generator, Seed)
import Random.Extra
import Random.List
import Rate exposing (Rate)
import Round
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared
    exposing
        ( Effect(..)
        , GenerationConfig
        , PlayType(..)
        , Settings
        , SharedModel
        )
import SubCmd exposing (SubCmd)
import Task
import Task.Parallel
import Temperature
import Ui.Button
import Ui.Theme
import View exposing (View)



---- INIT ----


init : SharedModel -> PlayType -> GenerationConfig -> ( World, SubCmd PlayingMsg Effect )
init sharedModel playType generationConfig =
    let
        ( generatedWorld, seed ) =
            let
                -- Filter out a civilization name if the player's chosen name matches
                worldWithPlayerDataFilteredOut : World
                worldWithPlayerDataFilteredOut =
                    case playType of
                        Participation ->
                            { emptyWorld
                                | availableCivilizationNames =
                                    List.filter (\name -> String.toLower (Data.Name.toString name) /= String.toLower (Data.Name.toString generationConfig.name))
                                        emptyWorld.availableCivilizationNames
                            }

                        Observation ->
                            emptyWorld
            in
            Random.step (generateGalaxy generationConfig worldWithPlayerDataFilteredOut) sharedModel.seed

        viableStartingPlanets : List ( EntityID, Orbit )
        viableStartingPlanets =
            List.sortWith planetOrbitPreference
                (List.filterMap
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
                    (Set.toList generatedWorld.planets)
                )

        ( inhabitedPlanets, finalSeed ) =
            Tuple.mapFirst
                (\xs ->
                    Maybe.withDefault Dict.empty
                        (Maybe.map (\( planetId, _ ) -> Dict.singleton planetId (Population.millions 7))
                            (List.head xs)
                        )
                )
                (Random.step (Random.List.shuffle viableStartingPlanets) seed)

        ( playerCivId, worldWithPlayerCiv ) =
            Logic.Entity.with
                ( Game.Components.civilizationHappinessSpec
                , Dict.map (\_ _ -> Percent.oneHundred) inhabitedPlanets
                )
                (Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, Rate.fromFloat 0.1 )
                    (Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, Rate.fromFloat 0.3 )
                        (Logic.Entity.with ( Game.Components.namedSpec, generationConfig.name )
                            (Logic.Entity.with ( Game.Components.civilizationPopulationSpec, inhabitedPlanets )
                                (Logic.Entity.Extra.create generatedWorld)
                            )
                        )
                    )
                )

        ( playerCiv, worldWithPlayerCivWithKnowledge ) =
            case playType of
                Participation ->
                    Tuple.mapFirst Just
                        (Logic.Entity.with
                            ( Data.Knowledge.spec
                            , Set.Any.fromList Data.Knowledge.comparableConfig
                                (KnowsOf playerCivId
                                    :: List.concatMap
                                        (\planetId ->
                                            case Logic.Component.get planetId worldWithPlayerCiv.parents of
                                                Nothing ->
                                                    [ KnowsOf planetId ]

                                                Just solarSystemId ->
                                                    case Logic.Component.get solarSystemId worldWithPlayerCiv.children of
                                                        Nothing ->
                                                            [ KnowsOf planetId ]

                                                        Just childrenIds ->
                                                            KnowsOf planetId :: List.map KnowsOf (Set.toList (Set.intersect childrenIds worldWithPlayerCiv.stars))
                                        )
                                        (Dict.keys inhabitedPlanets)
                                )
                            )
                            ( playerCivId, worldWithPlayerCiv )
                        )

                Observation ->
                    ( Nothing, generatedWorld )

        ( buildingKnowledgeState, parallelCmd ) =
            Task.Parallel.attemptList
                { onUpdates = BuildingKnowledge
                , onSuccess = KnowledgeBuilt
                , onFailure = \_ -> KnowledgeBuildFailure
                , tasks =
                    List.indexedMap
                        (\delay planetId ->
                            Task.map
                                (\() ->
                                    let
                                        solarSiblings : Set EntityID
                                        solarSiblings =
                                            getSolarSiblings worldWithPlayerCiv planetId
                                    in
                                    List.concat
                                        [ ---- Local solar knowledge
                                          List.map
                                            (\siblingId ->
                                                if Set.member siblingId worldWithPlayerCiv.stars then
                                                    -- No previous knowledge required
                                                    ( KnowsOf siblingId, [] )

                                                else
                                                    -- Requires Optics
                                                    ( KnowsOf siblingId, [ Set.Any.singleton Data.Knowledge.comparableConfig Optics ] )
                                            )
                                            (Set.toList solarSiblings)

                                        ---- Galactic knowledge
                                        -- Knowledge of extra solar stars only requires Optics
                                        , List.map
                                            (\nonSiblingStarId ->
                                                ( KnowsOf nonSiblingStarId, [ Set.Any.singleton Data.Knowledge.comparableConfig Optics ] )
                                            )
                                            (Set.toList (Set.diff worldWithPlayerCiv.stars solarSiblings))

                                        -- Knowledge of extra solar planets requires Optics and knowledge of at least 1 parent local star
                                        , List.map
                                            (\nonSiblingPlanetId ->
                                                ( KnowsOf nonSiblingPlanetId
                                                , List.map
                                                    (\foreignStarId ->
                                                        Set.Any.fromList Data.Knowledge.comparableConfig
                                                            [ Optics, KnowsOf foreignStarId ]
                                                    )
                                                    (Set.toList (Set.intersect worldWithPlayerCiv.stars (getSolarSiblings worldWithPlayerCiv nonSiblingPlanetId)))
                                                )
                                            )
                                            (Set.toList (Set.diff worldWithPlayerCiv.planets solarSiblings))

                                        ---- Other Civ Knowledge
                                        ]
                                )
                                (Process.sleep (toFloat delay))
                        )
                        (Set.toList worldWithPlayerCiv.planets)
                }
    in
    ( { worldWithPlayerCivWithKnowledge
        | playerCiv = playerCiv
        , playType = playType
        , civilizations =
            case playerCiv of
                Nothing ->
                    worldWithPlayerCiv.civilizations

                Just civId ->
                    Set.insert civId worldWithPlayerCiv.civilizations
        , zoom =
            Maybe.withDefault (25000 + (100 * 9460730000000000))
                (Maybe.map (\m -> m / 2)
                    (List.head
                        (List.reverse
                            (List.sort
                                (List.filterMap
                                    (\solarSystemId ->
                                        Maybe.map
                                            (\galacticPosition ->
                                                Length.inMeters (Point3d.distanceFrom Point3d.origin galacticPosition)
                                            )
                                            (Logic.Component.get solarSystemId worldWithPlayerCiv.galaxyPositions)
                                    )
                                    (Dict.keys (Logic.Component.toDict worldWithPlayerCiv.solarSystems))
                                )
                            )
                        )
                    )
                )
        , buildingKnowledgeState = buildingKnowledgeState
        , buildingKnowledge = Just ( 0, Set.size worldWithPlayerCiv.planets )
      }
    , SubCmd.batch
        [ SubCmd.effect (UpdateSeed finalSeed)
        , SubCmd.cmd parallelCmd
        ]
    )


getSolarSiblings : World -> EntityID -> Set EntityID
getSolarSiblings world child =
    case
        Maybe.andThen
            (\solarSystemId ->
                Maybe.map (Set.remove child)
                    (Logic.Component.get solarSystemId world.children)
            )
            (Logic.Component.get child world.parents)
    of
        Nothing ->
            Set.empty

        Just siblings ->
            siblings


{-| Oribits that are closer to 1AU are preferred
-}
planetOrbitPreference : ( EntityID, Orbit ) -> ( EntityID, Orbit ) -> Order
planetOrbitPreference ( _, orbitA ) ( _, orbitB ) =
    let
        distA : Float
        distA =
            Length.inAstronomicalUnits (Quantity.abs (Quantity.difference Length.astronomicalUnit (Data.Orbit.distance orbitA)))

        distB : Float
        distB =
            Length.inAstronomicalUnits (Quantity.abs (Quantity.difference Length.astronomicalUnit (Data.Orbit.distance orbitB)))
    in
    if distA > distB then
        GT

    else if distA < distB then
        LT

    else
        EQ



---- UPDATE ----


subscriptions : World -> Sub PlayingMsg
subscriptions world =
    case world.buildingKnowledge of
        Just _ ->
            Sub.none

        Nothing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Tick
                , Browser.Events.onResize (\_ _ -> WindowResized)
                ]


zoomMultiplier : SpaceFocus -> Float
zoomMultiplier focus =
    case focus of
        FGalaxy ->
            -- One light year is 9460730000000000, this is about 10 light years
            94607300000000000

        FSolarSystem _ ->
            10000000000

        FStar _ ->
            10000000000

        FPlanet _ ->
            1


update : SharedModel -> PlayingMsg -> World -> ( World, SubCmd PlayingMsg Effect )
update sharedModel msg world =
    case msg of
        BuildingKnowledge taskMsg ->
            let
                ( nextBuildingKnowledgeState, next ) =
                    Task.Parallel.updateList world.buildingKnowledgeState taskMsg
            in
            ( { world
                | buildingKnowledgeState = nextBuildingKnowledgeState
                , buildingKnowledge =
                    Maybe.map
                        (\( a, b ) -> ( a + 1, b ))
                        world.buildingKnowledge
              }
            , SubCmd.cmd next
            )

        KnowledgeBuilt knowledge ->
            ( { world
                | knowledgeTree = Data.Knowledge.addKnowledge (List.concat knowledge) world.knowledgeTree
                , buildingKnowledge = Nothing
              }
            , SubCmd.batch
                [ Galaxy3d.getGalaxyViewport GotGalaxyViewport
                , SubCmd.cmd
                    (Task.perform
                        (\() -> WindowResized)
                        (Process.sleep 1)
                    )
                ]
            )

        KnowledgeBuildFailure ->
            ( world, SubCmd.none )

        SetTickRate tickRate ->
            ( { world | tickRate = tickRate }, SubCmd.none )

        WindowResized ->
            ( world, Galaxy3d.getGalaxyViewport GotGalaxyViewport )

        GotSettingsVisible visible ->
            ( { world | settingsVisible = visible }, SubCmd.none )

        GotLocalSharedMessage settingsChange ->
            ( world
            , SubCmd.effect (GotSharedMessage settingsChange)
            )

        GotGalaxyViewport (Ok { viewport }) ->
            ( { world | galaxyViewSize = { width = viewport.width, height = viewport.height - 1 } }
            , SubCmd.none
            )

        GotGalaxyViewport (Err _) ->
            ( world, SubCmd.none )

        DeleteGalaxy ->
            ( world
            , SubCmd.effect Shared.DeleteGame
            )

        GotZoom zoomValue ->
            case Json.Decode.decodeValue decodeZoomEvent zoomValue of
                Ok delta ->
                    setZoom world (delta * zoomMultiplier world.spaceFocus)

                Err _ ->
                    ( world, SubCmd.none )

        GotZoomChange change ->
            setZoom world (change * zoomMultiplier world.spaceFocus)

        GotRotationChange change ->
            ( { world | viewRotation = toFloat (remainderBy 360 (floor (world.viewRotation + change))) }, SubCmd.none )

        SetSpaceFocus focus ->
            let
                zoomDist : Float
                zoomDist =
                    case focus of
                        FGalaxy ->
                            Maybe.withDefault (25000 + (100 * 9460730000000000))
                                (Maybe.map (\m -> m / 2)
                                    (List.head
                                        (List.reverse
                                            (List.sort
                                                (List.filterMap
                                                    (\solarSystemId ->
                                                        Maybe.map
                                                            (\galacticPosition ->
                                                                Length.inMeters (Point3d.distanceFrom Point3d.origin galacticPosition)
                                                            )
                                                            (Logic.Component.get solarSystemId world.galaxyPositions)
                                                    )
                                                    (Dict.keys (Logic.Component.toDict world.solarSystems))
                                                )
                                            )
                                        )
                                    )
                                )

                        FSolarSystem solarSystemId ->
                            -- a good number
                            Maybe.withDefault 1196782965600
                                (Maybe.map
                                    (\children ->
                                        let
                                            largestStarRadius : Float
                                            largestStarRadius =
                                                Maybe.withDefault 0
                                                    (List.head
                                                        (List.reverse
                                                            (List.sort
                                                                (List.filterMap
                                                                    (\starId ->
                                                                        Maybe.map
                                                                            (\starTemp ->
                                                                                Length.inMeters
                                                                                    (Point3d.distanceFrom Point3d.origin
                                                                                        (Point3d.fromMeters
                                                                                            { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                                                                            , y = 0
                                                                                            , z = 0
                                                                                            }
                                                                                        )
                                                                                    )
                                                                            )
                                                                            (Logic.Component.get starId world.starTemperature)
                                                                    )
                                                                    (Set.toList (Set.intersect children world.stars))
                                                                )
                                                            )
                                                        )
                                                    )

                                            largestPlanetOrbit : Float
                                            largestPlanetOrbit =
                                                Maybe.withDefault 0
                                                    (List.head
                                                        (List.reverse
                                                            (List.sort
                                                                (List.filterMap
                                                                    (\planetId ->
                                                                        Maybe.map
                                                                            (\orbit ->
                                                                                Length.inMeters (Data.Orbit.distance orbit)
                                                                            )
                                                                            (Logic.Component.get planetId world.orbits)
                                                                    )
                                                                    (Set.toList (Set.intersect children world.planets))
                                                                )
                                                            )
                                                        )
                                                    )
                                        in
                                        max largestStarRadius largestPlanetOrbit
                                    )
                                    (Logic.Component.get solarSystemId world.children)
                                )

                        FStar starId ->
                            -- a good number
                            Maybe.withDefault 1196782965600
                                (Maybe.map
                                    (\starTemp ->
                                        Length.inMeters
                                            (Point3d.distanceFrom Point3d.origin
                                                (Point3d.fromMeters
                                                    { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                                    , y = 0
                                                    , z = 0
                                                    }
                                                )
                                            )
                                    )
                                    (Logic.Component.get starId world.starTemperature)
                                )

                        FPlanet _ ->
                            1
            in
            ( { world
                | spaceFocus = focus
                , zoom = zoomDist
                , viewRotation = 0
              }
            , SubCmd.none
            )

        SetCivilizationFocus focus ->
            ( { world | civilizationFocus = focus }, SubCmd.none )

        GotViewStyle viewStyle ->
            ( { world | viewStyle = viewStyle }, Galaxy3d.getGalaxyViewport GotGalaxyViewport )

        Tick deltaMs ->
            let
                ( doTick, remainingTickTime, newStarDate ) =
                    let
                        baseTickTime : Float
                        baseTickTime =
                            -- 3 seconds
                            3000

                        remaining : Float
                        remaining =
                            world.remainingTimeForSystemUpdate + deltaMs
                    in
                    case world.tickRate of
                        Paused ->
                            ( False, remaining, world.starDate )

                        HalfSpeed ->
                            if remaining - baseTickTime * 2 >= 0 then
                                ( True, remaining - baseTickTime * 2, Quantity.plus (Data.EarthYear.starDates 1) world.starDate )

                            else
                                ( False, remaining, world.starDate )

                        Normal ->
                            if remaining - baseTickTime >= 0 then
                                ( True, remaining - baseTickTime, Quantity.plus (Data.EarthYear.starDates 1) world.starDate )

                            else
                                ( False, remaining, world.starDate )

                        Fast ->
                            if remaining - baseTickTime / 4 >= 0 then
                                ( True, remaining - baseTickTime / 4, Quantity.plus (Data.EarthYear.starDates 1) world.starDate )

                            else
                                ( False, remaining, world.starDate )

                        ExtraFast ->
                            if remaining - baseTickTime / 8 >= 0 then
                                ( True, remaining - baseTickTime / 8, Quantity.plus (Data.EarthYear.starDates 1) world.starDate )

                            else
                                ( False, remaining, world.starDate )

                updatedWorld : World
                updatedWorld =
                    { world
                        | starDate = newStarDate
                        , elapsedTime = world.elapsedTime + deltaMs
                        , remainingTimeForSystemUpdate = remainingTickTime
                    }
            in
            if doTick then
                Tuple.mapSecond (\seed -> SubCmd.effect (UpdateSeed seed))
                    (structureSystem
                        (civilUnrestSystem
                            (expansionSystem
                                (discoverySystem Data.Knowledge.spec
                                    ((\w -> ( w, sharedModel.seed ))
                                        (birthAndDeathSystem updatedWorld)
                                    )
                                )
                            )
                        )
                    )

            else
                ( updatedWorld
                , SubCmd.none
                )


structureSystem : ( World, Seed ) -> ( World, Seed )
structureSystem ( originalWorld, originalSeed ) =
    Logic.System.indexedFoldl3
        (\civId civPopulations civCharacteristics nameSource ( world, seed ) ->
            Random.step
                (Random.andThen
                    (\createMonument ->
                        if createMonument then
                            case Dict.keys civPopulations of
                                [] ->
                                    Random.constant world

                                planetId :: _ ->
                                    Random.andThen
                                        (\personName ->
                                            Random.map
                                                (\( structureType, name ) ->
                                                    Tuple.second
                                                        (Logic.Entity.with
                                                            ( Data.Structure.civilizationStructuresSpec
                                                            , { creators = civId
                                                              , creationDate = world.starDate
                                                              , type_ = structureType
                                                              , planet = planetId
                                                              , name = name
                                                              }
                                                            )
                                                            (Logic.Entity.Extra.create world)
                                                        )
                                                )
                                                (Data.Structure.random personName)
                                        )
                                        (Data.Name.randomPerson nameSource)

                        else
                            Random.constant world
                    )
                    (Random.Extra.oneIn (floor (200 - Data.EarthYear.distance world.starDate civCharacteristics.timeSinceLastMonument)))
                )
                seed
        )
        originalWorld.civilizationPopulations
        originalWorld.civilizationStyle
        originalWorld.civilizationPersonNameSource
        ( originalWorld, originalSeed )


setZoom : World -> Float -> ( World, SubCmd PlayingMsg Effect )
setZoom world delta =
    ( { world | zoom = max 5000000 (world.zoom + delta) }, SubCmd.none )


decodeZoomEvent : Json.Decode.Decoder Float
decodeZoomEvent =
    Json.Decode.field "deltaY" Json.Decode.float


expansionSystem : ( World, Seed ) -> ( World, Seed )
expansionSystem ( world, initialSeed ) =
    let
        planetsAndKnowledge : List { id : EntityID, knowledge : AnySet String Knowledge, populatedPlanets : List EntityID }
        planetsAndKnowledge =
            List.filterMap
                (\civId ->
                    Maybe.map2
                        (\knowledge population -> { id = civId, knowledge = knowledge, populatedPlanets = Dict.keys population })
                        (Logic.Component.get civId world.civilizationKnowledge)
                        (Logic.Component.get civId world.civilizationPopulations)
                )
                (Set.toList world.civilizations)
    in
    List.foldl
        (\civ ( nextWorld, nextSeed ) ->
            let
                totalPopulationSize : Population
                totalPopulationSize =
                    List.foldl
                        (\( _, planetPupulationCount ) ->
                            Population.plus planetPupulationCount
                        )
                        (Population.millions 0)
                        (Dict.toList (Maybe.withDefault Dict.empty (Logic.Component.get civ.id world.civilizationPopulations)))
            in
            if Quantity.lessThan Population.trillion totalPopulationSize then
                ( nextWorld, nextSeed )

            else
                let
                    filterBySameSolarSystem : EntityID -> Maybe (List EntityID)
                    filterBySameSolarSystem planetId =
                        Maybe.andThen
                            (\solarSystemId ->
                                Maybe.map
                                    (\childIds ->
                                        Set.toList
                                            (Set.filter
                                                (\childId ->
                                                    (childId /= planetId) && Set.member childId world.planets
                                                )
                                                childIds
                                            )
                                    )
                                    (Logic.Component.get solarSystemId world.children)
                            )
                            (Logic.Component.get planetId world.parents)

                    allPopulatedPlanets : Set EntityID
                    allPopulatedPlanets =
                        Set.fromList
                            (List.concatMap (\( _, populations ) -> Dict.keys populations)
                                (Logic.Component.toList world.civilizationPopulations)
                            )

                    allAvailablePlanets : Set EntityID
                    allAvailablePlanets =
                        (\allPlanets -> Set.diff allPlanets allPopulatedPlanets)
                            (Set.filter
                                (\planetId ->
                                    case Logic.Component.get planetId world.planetTypes of
                                        Nothing ->
                                            False

                                        Just Gas ->
                                            False

                                        Just Rocky ->
                                            True
                                )
                                world.planets
                            )

                    planetsInSameSolarSystem : List EntityID
                    planetsInSameSolarSystem =
                        List.concat (List.filterMap filterBySameSolarSystem civ.populatedPlanets)

                    possiblePlanetsToExpandInto : List ( Float, EntityID )
                    possiblePlanetsToExpandInto =
                        if Data.Knowledge.knows civ.knowledge FTLSpaceTravel then
                            List.map
                                (\planetId ->
                                    ( if List.member planetId planetsInSameSolarSystem then
                                        2.0

                                      else
                                        1.0
                                    , planetId
                                    )
                                )
                                (Set.toList allAvailablePlanets)

                        else if Data.Knowledge.knows civ.knowledge InterplanetarySpaceTravel then
                            List.filterMap
                                (\planetId ->
                                    if Set.member planetId allAvailablePlanets then
                                        Just ( 1.0, planetId )

                                    else
                                        Nothing
                                )
                                planetsInSameSolarSystem

                        else
                            []
                in
                case possiblePlanetsToExpandInto of
                    [] ->
                        ( nextWorld, nextSeed )

                    _ ->
                        Random.step (possiblyExpandToPlanet civ.id possiblePlanetsToExpandInto world) nextSeed
        )
        ( world, initialSeed )
        planetsAndKnowledge


possiblyExpandToPlanet : EntityID -> List ( Float, EntityID ) -> World -> Generator World
possiblyExpandToPlanet civId possiblePlanetsToExpandInto world =
    Random.map
        (\maybePlanetId ->
            case maybePlanetId of
                Nothing ->
                    world

                Just planetId ->
                    { world
                        | civilizationPopulations =
                            Logic.Component.update civId
                                (Dict.insert planetId Population.million)
                                world.civilizationPopulations
                    }
        )
        (weightedChoose possiblePlanetsToExpandInto)


weightedChoose : List ( Float, a ) -> Generator (Maybe a)
weightedChoose items =
    case items of
        [] ->
            Random.constant Nothing

        first :: rest ->
            Random.map Just (Random.weighted first rest)


civilUnrestSystem : ( World, Seed ) -> ( World, Seed )
civilUnrestSystem ( world, initialSeed ) =
    let
        revoltingCivs : List ( EntityID, EntityID )
        revoltingCivs =
            List.concatMap
                (\civId ->
                    Maybe.withDefault []
                        (Maybe.map
                            (Dict.foldl
                                (\planetId planetHappiness planetsRevolting ->
                                    if Quantity.lessThan (Percent.fromFloat 0.15) planetHappiness then
                                        ( civId, planetId ) :: planetsRevolting

                                    else
                                        planetsRevolting
                                )
                                []
                            )
                            (Logic.Component.get civId world.civilizationHappiness)
                        )
                )
                (Set.toList world.civilizations)
    in
    Random.step (generateRevoltingCivs revoltingCivs world) initialSeed


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
                    Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
                        (Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, civDetails.mortalityRate )
                            (Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, civDetails.reproductionRate )
                                (Logic.Entity.with
                                    ( Game.Components.namedSpec
                                    , Data.Name.fromString ("Renegade " ++ Data.Name.toString civDetails.name)
                                    )
                                    (Logic.Entity.with
                                        ( Game.Components.civilizationPopulationSpec
                                        , Dict.singleton planetId
                                            (Maybe.withDefault (Population.millions 3)
                                                (Dict.get planetId civDetails.occupiedPlanets)
                                            )
                                        )
                                        (Logic.Entity.Extra.create world)
                                    )
                                )
                            )
                        )

                ( _, worldWithNewCivWithKnowledge ) =
                    Logic.Entity.with
                        ( Data.Knowledge.spec
                        , Set.Any.union
                            (Set.Any.fromList Data.Knowledge.comparableConfig
                                (List.concat
                                    [ [ KnowsOf oldCivId
                                      , KnowsOf civId
                                      , KnowsOf planetId
                                      ]
                                    , Set.foldl (\id res -> KnowsOf id :: res)
                                        []
                                        (Set.intersect
                                            (getSolarSiblings worldWithNewCiv planetId)
                                            worldWithNewCiv.stars
                                        )
                                    ]
                                )
                            )
                            knowledge
                        )
                        ( civId, worldWithNewCiv )
            in
            { worldWithNewCivWithKnowledge
                | civilizations = Set.insert civId worldWithNewCivWithKnowledge.civilizations
                , civilizationKnowledge =
                    Logic.Component.update oldCivId
                        (\_ ->
                            Set.Any.union
                                -- Don't forget the planets the old civ lives on!
                                (case Logic.Component.get oldCivId worldWithNewCivWithKnowledge.civilizationPopulations of
                                    Nothing ->
                                        Set.Any.empty

                                    Just populatedPlanets ->
                                        Set.Any.fromList Data.Knowledge.comparableConfig
                                            (List.map KnowsOf (Dict.keys populatedPlanets))
                                )
                                -- Don't forget your civ-self, your planet, or the revolting civ!
                                (Set.Any.union
                                    (Set.Any.fromList Data.Knowledge.comparableConfig
                                        [ KnowsOf civId
                                        , KnowsOf oldCivId
                                        , KnowsOf planetId
                                        ]
                                    )
                                    oldCivNewKnowledge
                                )
                        )
                        worldWithNewCivWithKnowledge.civilizationKnowledge
            }
        )
        (Percent.random 0.95 1.0)
        (Random.andThen
            (\percent -> dropRandom Data.Knowledge.comparableConfig percent civDetails.knowledge)
            (Percent.random 0.05 0.25)
        )
        (Random.andThen
            (\percent -> dropRandom Data.Knowledge.comparableConfig percent civDetails.knowledge)
            (Percent.random 0.01 0.05)
        )


dropRandom : { r | fromComparable : comparable -> b, toComparable : b -> comparable } -> Percent a -> AnySet comparable b -> Generator (AnySet comparable b)
dropRandom anySetConfig percent set =
    let
        setList : List b
        setList =
            Set.Any.toList anySetConfig set

        toDrop : Int
        toDrop =
            floor (Percent.toFloat (Quantity.divideBy (toFloat (List.length setList)) (Quantity.multiplyBy 100.0 percent)))
    in
    Random.map (\a -> Set.Any.fromList anySetConfig (List.drop toDrop a))
        (Random.List.shuffle setList)


discoverySystem : Spec (AnySet String Knowledge) World -> ( World, Seed ) -> ( World, Seed )
discoverySystem knowledge ( world, initialSeed ) =
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
            , world : World
            , logs : List Log
            }
        updates =
            Array.foldl possiblyGainKnowledge
                { index = 0
                , updatedKnowledge = initialUpdatedKnowledge
                , seed = initialSeed
                , world = world
                , logs = []
                }
                (knowledge.get world)
    in
    ( knowledge.set updates.updatedKnowledge { world | eventLog = updates.logs ++ world.eventLog }
    , updates.seed
    )


possiblyGainKnowledge :
    Maybe (AnySet String Knowledge)
    ->
        { index : Int
        , updatedKnowledge : Array (Maybe (AnySet String Knowledge))
        , seed : Seed
        , world : World
        , logs : List Log
        }
    ->
        { index : Int
        , updatedKnowledge : Array (Maybe (AnySet String Knowledge))
        , seed : Seed
        , world : World
        , logs : List Log
        }
possiblyGainKnowledge maybeCivKnowledge ({ index, updatedKnowledge, seed, world } as updates) =
    case maybeCivKnowledge of
        Nothing ->
            { updates | index = index + 1, updatedKnowledge = Array.set index Nothing updatedKnowledge }

        Just civKnowledge ->
            Maybe.withDefault { updates | index = index + 1 }
                (Maybe.map3
                    (\civStyle civName civNameSource ->
                        let
                            ( ( updatedCivKnowledge, maybeLog ), newSeed ) =
                                gainRandomKnowledge
                                    civKnowledge
                                    index
                                    updatedKnowledge
                                    maybeCivKnowledge
                                    seed
                                    world
                                    civName
                                    civStyle
                                    civNameSource
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
                    )
                    (Maybe.andThen identity (Array.get index world.civilizationStyle))
                    (Maybe.andThen identity (Array.get index world.named))
                    (Maybe.andThen identity (Array.get index world.civilizationPersonNameSource))
                )


gainRandomKnowledge :
    AnySet String Knowledge
    -> Int
    -> Array (Maybe (AnySet String Knowledge))
    -> Maybe (AnySet String Knowledge)
    -> Seed
    -> World
    -> Name
    -> Data.Civilization.Characteristics
    -> Markov.String.MarkovString
    -> ( ( Array (Maybe (AnySet String Knowledge)), Maybe Log ), Seed )
gainRandomKnowledge civKnowledge index allCivsKnowledge maybeCivKnowledge seed world civName civStyle nameSource =
    Random.step
        (Random.andThen
            (\gainsKnowledge ->
                if gainsKnowledge then
                    let
                        canBeLearned : List Knowledge
                        canBeLearned =
                            Set.Any.toList Data.Knowledge.comparableConfig (Data.Knowledge.canBeLearned world.knowledgeTree civKnowledge)
                    in
                    case canBeLearned of
                        [] ->
                            Random.constant ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )

                        first :: rest ->
                            Random.map2
                                (\personName knowledgeGained ->
                                    ( Array.set index (Just (Set.Any.insert Data.Knowledge.comparableConfig knowledgeGained civKnowledge)) allCivsKnowledge
                                    , Just
                                        { time = world.starDate
                                        , description = Data.Name.enhancedEventDescription civName personName ++ " gained new knowledge."
                                        , civilizationId = index
                                        }
                                    )
                                )
                                (Data.Name.randomPerson nameSource)
                                (Random.uniform first rest)

                else
                    Random.constant ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )
            )
            (Random.Extra.oneIn (100 - floor (civStyle.cooperationVsCompetition * 5)))
        )
        seed


birthAndDeathSystem : System World
birthAndDeathSystem =
    Logic.System.step5
        (\( reproductionRate, _ ) ( mortalityRate, _ ) ( populationSizes, setPopulationSize ) ( civDensity, _ ) ( knowledge, _ ) ->
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
                        Quantity.min
                            (if Data.Knowledge.knows knowledge MegaCities then
                                Population.trillions (10 * civDensity)

                             else if Data.Knowledge.knows knowledge Cities then
                                Population.billions (10 * civDensity)

                             else if Data.Knowledge.knows knowledge Villages then
                                Population.millions (10 * civDensity)

                             else
                                Population.millions civDensity
                            )
                            (Population.plus (Population.difference populationSize deaths) births)
                    )
                    populationSizes
                )
        )
        Game.Components.civilizationReproductionRateSpec
        Game.Components.civilizationMortalityRateSpec
        Game.Components.civilizationPopulationSpec
        Game.Components.civilizationDensitySpec
        Data.Knowledge.spec


generateGalaxy : GenerationConfig -> World -> Generator World
generateGalaxy config model =
    Random.map Tuple.second
        (generateManyEntities
            config.minSolarSystemsToGenerate
            config.maxSolarSystemsToGenerate
            model
            (generateSolarSystem config)
        )


generateSolarSystem : GenerationConfig -> ( EntityID, World ) -> Generator ( EntityID, World )
generateSolarSystem config ( solarSystemId, world ) =
    Random.andThen
        (\( starIds, starWorld ) ->
            Random.map2
                (\( planetIds, finalWorld ) galacticPosition ->
                    Logic.Entity.with ( Game.Components.solarSystemSpec, SolarSystem )
                        (Logic.Entity.with ( Game.Components.positionSpec, galacticPosition )
                            (Logic.Entity.with ( Game.Components.childrenSpec, Set.union planetIds starIds )
                                ( solarSystemId
                                , finalWorld
                                )
                            )
                        )
                )
                (generateManyEntities
                    config.minPlanetsPerSolarSystemToGenerate
                    config.maxPlanetsPerSolarSystemToGenerate
                    starWorld
                    (generatePlanet solarSystemId)
                )
                generateGalacticPosition
        )
        (Random.andThen
            (\starCount ->
                generateManyEntities
                    starCount
                    starCount
                    world
                    (generateStar solarSystemId)
            )
            (Random.weighted (List.Nonempty.head config.starCounts) (List.Nonempty.tail config.starCounts))
        )


generateGalacticPosition : Generator (Point3d Meters LightYear)
generateGalacticPosition =
    Random.map2
        (\rand1 rand2 ->
            let
                radius : Float
                radius =
                    Length.inMeters (Length.lightYears 50000)

                u2 : Float
                u2 =
                    radius * rand2

                u1 : Float
                u1 =
                    2 * pi * rand1
            in
            Point3d.fromMeters
                { x = u2 * cos u1
                , y = u2 * sin u1
                , z = 0
                }
        )
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)


generateStar : EntityID -> ( EntityID, World ) -> Generator ( EntityID, World )
generateStar solarSystemId ( starId, world ) =
    Random.map
        (\starTemperature ->
            Tuple.mapSecond (\w -> { w | stars = Set.insert starId w.stars }) (Logic.Entity.with ( Game.Components.parentSpec, solarSystemId ) (Logic.Entity.with ( Data.Star.temperatureSpec, starTemperature ) ( starId, world )))
        )
        Data.Star.random


generatePlanet : EntityID -> ( EntityID, World ) -> Generator ( EntityID, World )
generatePlanet solarSystemId ( planetId, world ) =
    Random.andThen
        (\( planetType, waterPercent ) ->
            Random.map4
                (\orbitDistance orbitPeriod size updatedWorld ->
                    Tuple.mapSecond (\w -> { w | planets = Set.insert planetId w.planets })
                        (Logic.Entity.with ( Game.Components.parentSpec, solarSystemId )
                            (Logic.Entity.with ( Game.Components.planetSizeSpec, size )
                                (Logic.Entity.with ( Game.Components.waterSpec, waterPercent )
                                    (Logic.Entity.with
                                        ( Game.Components.orbitSpec
                                        , Data.Orbit.create
                                            { distance = Length.astronomicalUnits orbitDistance
                                            , period = Data.EarthYear.earthYears orbitPeriod
                                            }
                                        )
                                        (Logic.Entity.with ( Game.Components.planetTypeSpec, planetType )
                                            ( planetId, updatedWorld )
                                        )
                                    )
                                )
                            )
                        )
                )
                (case planetType of
                    Rocky ->
                        Random.float 0.25 7

                    Gas ->
                        Random.float 5 40
                )
                (Random.float 0.25 350)
                (generatePlanetRadius planetType)
                (attemptToGenerateCivilization planetType planetId world)
        )
        (Random.map2 Tuple.pair
            (Random.uniform Rocky [ Gas ])
            (Percent.random 0.0 100.0)
        )


attemptToGenerateCivilization : CelestialBodyForm -> EntityID -> World -> Generator World
attemptToGenerateCivilization planetType planetId world =
    if planetType == Rocky then
        Random.andThen
            (\shouldCreateCiv ->
                if shouldCreateCiv then
                    Random.andThen
                        (\( maybeName, worldWithFewerNames ) ->
                            case maybeName of
                                Nothing ->
                                    Random.constant worldWithFewerNames

                                Just name ->
                                    generateCivilization worldWithFewerNames planetId name
                        )
                        (generateCivilizationName world)

                else
                    Random.constant world
            )
            (Random.Extra.oneIn 10)

    else
        Random.constant world


generateCivilization : World -> EntityID -> Name -> Generator World
generateCivilization worldWithFewerNames planetId name =
    Random.Extra.andMap Data.Name.randomPersonSource
        (Random.Extra.andMap (Random.float 0.0 1.0)
            (Random.Extra.andMap (Random.float 0.7 1.3)
                (Random.Extra.andMap (Percent.random 0.9 1.0)
                    (Random.Extra.andMap (Rate.random 0.1 0.2)
                        (Random.Extra.andMap (Rate.random 0.2 0.3)
                            (Random.map
                                (\initialPopulationSize reproductionRate mortalityRate initialHappiness civDensity coopVsComp nameSource ->
                                    let
                                        ( civId, worldWithNewCiv ) =
                                            Logic.Entity.with
                                                ( Game.Components.civilizationPersonNameSourceSpec
                                                , nameSource
                                                )
                                                (Logic.Entity.with
                                                    ( Data.Civilization.styleSpec
                                                    , { cooperationVsCompetition = coopVsComp
                                                      , timeSinceLastMonument = worldWithFewerNames.starDate
                                                      }
                                                    )
                                                    (Logic.Entity.with ( Game.Components.civilizationDensitySpec, civDensity )
                                                        (Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
                                                            (Logic.Entity.with
                                                                ( Game.Components.civilizationMortalityRateSpec, mortalityRate )
                                                                (Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, reproductionRate )
                                                                    (Logic.Entity.with ( Game.Components.namedSpec, name )
                                                                        (Logic.Entity.with
                                                                            ( Game.Components.civilizationPopulationSpec
                                                                            , Dict.singleton planetId (Population.millions initialPopulationSize)
                                                                            )
                                                                            (Logic.Entity.Extra.create worldWithFewerNames)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )

                                        ( _, worldWithNewCivWithKnowledge ) =
                                            Logic.Entity.with
                                                ( Data.Knowledge.spec
                                                , Set.Any.fromList Data.Knowledge.comparableConfig
                                                    -- All civs know of themselves and their home planet
                                                    [ KnowsOf planetId
                                                    , KnowsOf civId
                                                    ]
                                                )
                                                ( civId, worldWithNewCiv )
                                    in
                                    { worldWithNewCivWithKnowledge | civilizations = Set.insert civId worldWithNewCivWithKnowledge.civilizations }
                                )
                                (Random.float 0.3 0.8)
                            )
                        )
                    )
                )
            )
        )


generateCivilizationName : World -> Generator ( Maybe Name, World )
generateCivilizationName world =
    Random.map
        (\( chosenName, remainingNames ) ->
            ( chosenName, { world | availableCivilizationNames = remainingNames } )
        )
        (Random.List.choose world.availableCivilizationNames)


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
    Random.andThen
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
        (Random.int (min minimum maximum) (max minimum maximum))


generateEntity : World -> (( EntityID, World ) -> Generator ( EntityID, World )) -> Generator ( EntityID, World )
generateEntity world fn =
    fn (Logic.Entity.Extra.create world)



---- VIEW ----


view : SharedModel -> World -> View PlayingMsg
view sharedModel world =
    { title = "Hello Space!"
    , body = viewPlaying sharedModel (filterWorldByKnowledge world)
    }


filterWorldByKnowledge : World -> World
filterWorldByKnowledge world =
    case ( world.playType, world.playerCiv ) of
        ( Participation, Just playerCivId ) ->
            case Logic.Component.get playerCivId world.civilizationKnowledge of
                Nothing ->
                    world

                Just playerCivKnowledge ->
                    let
                        knowsOfIds : Set EntityID
                        knowsOfIds =
                            Set.fromList
                                (List.filterMap
                                    (\knowledge ->
                                        case knowledge of
                                            KnowsOf id ->
                                                Just id

                                            _ ->
                                                Nothing
                                    )
                                    (Set.Any.toList Data.Knowledge.comparableConfig playerCivKnowledge)
                                )

                        knownPlanets : Set EntityID
                        knownPlanets =
                            Set.intersect knowsOfIds world.planets

                        knownStars : Set EntityID
                        knownStars =
                            Set.intersect knowsOfIds world.stars

                        knownParents : Set EntityID
                        knownParents =
                            Set.fromList
                                (List.filterMap (\id -> Logic.Component.get id world.parents)
                                    (Set.toList (Set.union knownPlanets knownStars))
                                )
                    in
                    { world
                        | planets = knownPlanets
                        , stars = knownStars
                        , solarSystems =
                            Logic.Component.fromDict
                                (Dict.filter (\id _ -> Set.member id knowsOfIds || Set.member id knownParents)
                                    (Logic.Component.toDict world.solarSystems)
                                )
                        , civilizations = Set.intersect knowsOfIds world.civilizations
                    }

        _ ->
            world


viewPlaying : SharedModel -> World -> Element PlayingMsg
viewPlaying sharedModel world =
    case world.buildingKnowledge of
        Just ( completed, total ) ->
            column
                [ centerX
                , centerY
                ]
                [ text "Generating Galaxy"
                , row
                    [ width (px 300)
                    , inFront
                        (el
                            [ centerX
                            , centerY
                            , Background.color (rgba 1 1 1 0.5)
                            , Border.rounded 4
                            , padding 4
                            ]
                            (text (String.fromInt (completed * 100 // total) ++ "%"))
                        )
                    ]
                    [ el
                        [ Background.gradient
                            { angle = pi / 2
                            , steps =
                                [ rgb 1 0 1
                                , rgb 0.5 0.5 1
                                , rgb 0.1 1 0.1
                                ]
                            }
                        , width (fillPortion completed)
                        , height (px 35)
                        , Border.widthEach
                            { top = 3
                            , bottom = 3
                            , left = 3
                            , right = 0
                            }
                        ]
                        none
                    , el
                        [ width (fillPortion (total - completed))
                        , height (px 35)
                        , Border.widthEach
                            { top = 3
                            , bottom = 3
                            , left = 0
                            , right = 3
                            }
                        ]
                        none
                    ]
                ]

        Nothing ->
            column
                [ width fill
                , height fill
                , inFront
                    (case world.settingsVisible of
                        Hidden ->
                            none

                        Visible ->
                            map GotLocalSharedMessage (Shared.viewSettings sharedModel.settings)
                    )
                ]
                [ viewControls world
                , (case world.viewStyle of
                    ThreeD ->
                        column

                    TwoD ->
                        row
                  )
                    [ width fill
                    , height fill
                    , scrollbarY
                    , padding 16
                    , spacing 8
                    ]
                    [ el
                        ((case world.viewStyle of
                            ThreeD ->
                                []

                            TwoD ->
                                [ scrollbarY ]
                         )
                            ++ [ alignTop
                               , width fill
                               , height fill
                               , Border.solid
                               , Border.width 1
                               ]
                        )
                        (case world.spaceFocus of
                            FGalaxy ->
                                viewGalaxy world

                            FSolarSystem id ->
                                case Logic.Component.get id world.solarSystems of
                                    Just SolarSystem ->
                                        viewSlice
                                            [ Ui.Button.default
                                                { label = text "View Galaxy"
                                                , onPress = Just (SetSpaceFocus FGalaxy)
                                                }
                                            ]
                                            (viewSolarSystemDetailed sharedModel.settings world id)

                                    Nothing ->
                                        text "Missing solar system"

                            FStar starId ->
                                if Set.member starId world.stars then
                                    viewSlice
                                        [ Ui.Button.default
                                            { label = text "View Galaxy"
                                            , onPress = Just (SetSpaceFocus FGalaxy)
                                            }
                                        , Ui.Button.default
                                            { label = text "View System"
                                            , onPress =
                                                Maybe.map (\id -> SetSpaceFocus (FSolarSystem id))
                                                    (Logic.Component.get starId world.parents)
                                            }
                                        ]
                                        (viewStarDetailed world starId)

                                else
                                    text "Missing star"

                            FPlanet planetId ->
                                if Set.member planetId world.planets then
                                    viewSlice
                                        [ Ui.Button.default
                                            { label = text "View Galaxy"
                                            , onPress = Just (SetSpaceFocus FGalaxy)
                                            }
                                        , Ui.Button.default
                                            { label = text "View System"
                                            , onPress =
                                                Maybe.map (\id -> SetSpaceFocus (FSolarSystem id))
                                                    (Logic.Component.get planetId world.parents)
                                            }
                                        ]
                                        (viewPlanetDetailed world planetId)

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


viewControls : World -> Element PlayingMsg
viewControls world =
    row
        [ padding 16
        , spacing 16
        , width fill
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
        , text (Data.EarthYear.formatAsStarDate world.starDate)
        , Ui.Button.default
            (case world.viewStyle of
                ThreeD ->
                    { label = text "View 2D Glaxy"
                    , onPress = Just (GotViewStyle TwoD)
                    }

                TwoD ->
                    { label = text "View 3D Glaxy"
                    , onPress = Just (GotViewStyle ThreeD)
                    }
            )
        , el [ width fill ] none
        , Ui.Button.negative
            { label = text "Delete Galaxy"
            , onPress = Just DeleteGalaxy
            }
        , el [ width fill ] none
        , el [ alignRight ]
            (Ui.Button.default
                { label = text "⚙"
                , onPress =
                    Just
                        (case world.settingsVisible of
                            Visible ->
                                GotSettingsVisible Hidden

                            Hidden ->
                                GotSettingsVisible Visible
                        )
                }
            )
        ]


viewSlice : List (Element PlayingMsg) -> Element PlayingMsg -> Element PlayingMsg
viewSlice menuItems slice =
    el
        [ height fill
        , width fill
        , inFront (row [ padding 8, spacing 8 ] menuItems)
        ]
        slice


viewGalaxy : World -> Element PlayingMsg
viewGalaxy world =
    case world.viewStyle of
        ThreeD ->
            Galaxy3d.viewGalaxy
                { onPressSolarSystem = \id -> SetSpaceFocus (FSolarSystem id)
                , onZoom = GotZoom
                , onZoomPress = GotZoomChange
                , onRotationPress = GotRotationChange
                , focusedCivilization =
                    case world.civilizationFocus of
                        FAll ->
                            Nothing

                        FOne id ->
                            Just id
                }
                world

        TwoD ->
            Galaxy2d.viewGalaxy
                { onPressSolarSystem = \id -> SetSpaceFocus (FSolarSystem id)
                , onPressCivilization = \id -> SetCivilizationFocus (FOne id)
                , focusedCivilization =
                    case world.civilizationFocus of
                        FAll ->
                            Nothing

                        FOne id ->
                            Just id
                }
                world


viewSolarSystemDetailed : Settings -> World -> EntityID -> Element PlayingMsg
viewSolarSystemDetailed settings world solarSystemId =
    let
        ( stars, planets ) =
            Maybe.withDefault ( Set.empty, Set.empty )
                (Maybe.map
                    (\children ->
                        ( Set.intersect children world.stars
                        , Set.intersect children world.planets
                        )
                    )
                    (Logic.Component.get solarSystemId world.children)
                )
    in
    case world.viewStyle of
        ThreeD ->
            Galaxy3d.viewSolarSystem
                { onPressStar = Just (\id -> SetSpaceFocus (FStar id))
                , onPressPlanet = Just (\id -> SetSpaceFocus (FPlanet id))
                , onZoom = Just GotZoom
                , onZoomPress = Just GotZoomChange
                , onRotationPress = Just GotRotationChange
                , focusedCivilization =
                    case world.civilizationFocus of
                        FAll ->
                            Nothing

                        FOne id ->
                            Just id
                , stars = stars
                , planets = planets
                }
                settings
                world

        TwoD ->
            Galaxy2d.viewSolarSystem
                { onPressPlanet = \id -> SetSpaceFocus (FPlanet id)
                , onPressStar = \id -> SetSpaceFocus (FStar id)
                , onPressCivilization = \id -> SetCivilizationFocus (FOne id)
                , focusedCivilization =
                    case world.civilizationFocus of
                        FAll ->
                            Nothing

                        FOne id ->
                            Just id
                }
                solarSystemId
                stars
                world
                planets


viewStarDetailed : World -> EntityID -> Element PlayingMsg
viewStarDetailed model starId =
    case Logic.Component.get starId model.starTemperature of
        Nothing ->
            text "Your star is missing!"

        Just temp ->
            column
                [ spacing 8
                , paddingEach
                    { top = 64
                    , left = 8
                    , bottom = 8
                    , right = 8
                    }
                ]
                [ text ("Name: S_" ++ String.fromInt starId)
                , text ("Temperature: " ++ String.fromInt (round (Temperature.inKelvins temp)) ++ "K")
                ]


viewPlanetDetailed : World -> EntityID -> Element PlayingMsg
viewPlanetDetailed world planetId =
    case Logic.Component.get planetId world.planetTypes of
        Nothing ->
            text "Your planet is missing!"

        Just planetType ->
            let
                civsOnPlanet : List ( EntityID, Name )
                civsOnPlanet =
                    List.filterMap
                        (\civId ->
                            Maybe.andThen
                                (\dictPlanetPopulatiopns ->
                                    if Dict.member planetId dictPlanetPopulatiopns then
                                        Maybe.map (Tuple.pair civId)
                                            (Logic.Component.get civId world.named)

                                    else
                                        Nothing
                                )
                                (Logic.Component.get civId world.civilizationPopulations)
                        )
                        (Set.toList world.civilizations)
            in
            column
                [ spacing 8
                , paddingEach
                    { top = 64
                    , left = 8
                    , bottom = 8
                    , right = 8
                    }
                ]
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
                        column [ spacing 4 ]
                            (List.map
                                (\( civId, name ) ->
                                    Ui.Button.default
                                        { label = text (Data.Name.toString name)
                                        , onPress = Just (SetCivilizationFocus (FOne civId))
                                        }
                                )
                                civs
                            )
                , text "Structures:"
                , column []
                    (List.filterMap
                        (\( _, structure ) ->
                            if structure.planet == planetId then
                                Just (text (Data.Structure.toString structure))

                            else
                                Nothing
                        )
                        (Logic.Component.toList world.civilizationStructures)
                    )
                ]


viewCivilizations : World -> Element PlayingMsg
viewCivilizations world =
    column
        [ spacing 8
        , alignTop
        , width fill
        , height fill
        , scrollbarY
        ]
        (List.map (viewCivilizationSimple world) (Set.toList world.civilizations))


viewCivilizationSimple : World -> EntityID -> Element PlayingMsg
viewCivilizationSimple world civId =
    case Logic.Component.get civId world.named of
        Nothing ->
            text "Civilization is missing"

        Just name ->
            row
                [ spacing 8
                , width fill
                , Background.color Ui.Theme.nearlyWhite
                ]
                [ Ui.Button.inspect
                    (Just (SetCivilizationFocus (FOne civId)))
                , text (Data.Name.toString name)
                ]


viewCivilizationDetailed : World -> EntityID -> Element PlayingMsg
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
        , Background.color Ui.Theme.nearlyWhite
        ]
        (case getCivilizationDetails world civId of
            Nothing ->
                [ text "Civ is missing" ]

            Just details ->
                let
                    totalPopulationSize : Population
                    totalPopulationSize =
                        List.foldl (\( _, planetPupulationCount ) -> Population.plus planetPupulationCount) (Population.millions 0) (Dict.toList details.occupiedPlanets)
                in
                [ Ui.Button.default
                    { label = text "Back"
                    , onPress = Just (SetCivilizationFocus FAll)
                    }
                , text ("The " ++ Data.Name.toString (Data.Name.plurualize details.name) ++ " have " ++ populationToString totalPopulationSize ++ " citizens.")
                , text ("Happiness " ++ happinessToString (averageCivilizationHappiness details.happiness))
                , case Logic.Component.get civId world.civilizationStyle of
                    Nothing ->
                        none

                    Just civStyle ->
                        column
                            [ spacing 4, width (px 400) ]
                            [ row [ spacing 64, width fill ]
                                [ text "Cooperative"
                                , el [ centerX ] (text "or")
                                , el [ alignRight ] (text "Competitive")
                                ]
                            , el
                                [ Border.width 1
                                , width fill
                                , inFront
                                    (el
                                        [ height (px 16)
                                        , Border.width 2
                                        , moveUp 8
                                        , moveRight (civStyle.cooperationVsCompetition * 400)
                                        ]
                                        none
                                    )
                                ]
                                none
                            ]
                , text "They occupy planets:"
                , column []
                    (List.map
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
                        (Dict.toList details.occupiedPlanets)
                    )
                , text "Logs"
                , column [ spacing 4 ] (List.map viewLog details.logs)
                ]
        )


averageCivilizationHappiness : Dict EntityID (Percent Happiness) -> Percent Happiness
averageCivilizationHappiness happiness =
    Quantity.divideBy (toFloat (Dict.size happiness)) (List.foldl (\( _, happinessPerPlanet ) -> Quantity.plus happinessPerPlanet) Percent.zero (Dict.toList happiness))


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


viewLog : Log -> Element PlayingMsg
viewLog log =
    column
        [ Border.solid
        , Border.width 1
        , padding 4
        ]
        [ text (Data.EarthYear.formatAsStarDate log.time)
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
    , name : Name
    , happiness : Dict EntityID (Percent Happiness)
    }


getCivilizationDetails : World -> EntityID -> Maybe CivilizationDetails
getCivilizationDetails world civId =
    Maybe.map4
        (\reproductionRate mortalityRate name happiness ->
            { occupiedPlanets =
                Maybe.withDefault Dict.empty (Logic.Component.get civId world.civilizationPopulations)
            , reproductionRate = reproductionRate
            , mortalityRate = mortalityRate
            , knowledge =
                Maybe.withDefault Set.Any.empty (Logic.Component.get civId world.civilizationKnowledge)
            , logs = List.filter (\log -> log.civilizationId == civId) world.eventLog
            , name = name
            , happiness = happiness
            }
        )
        (Logic.Component.get civId world.civilizationReproductionRates)
        (Logic.Component.get civId world.civilizationMortalityRates)
        (Logic.Component.get civId world.named)
        (Logic.Component.get civId world.civilizationHappiness)
