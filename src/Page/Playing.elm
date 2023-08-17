module Page.Playing exposing
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
import Data.Name exposing (Name, NameSource)
import Data.Orbit exposing (Orbit)
import Data.Star
import Data.Structure
import Dict exposing (Dict)
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
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Meters)
import List.Nonempty
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
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
import Route exposing (GenerationConfig, PlayType(..))
import Set exposing (Set)
import Set.Any exposing (AnySet)
import Shared
    exposing
        ( Effect(..)
        , Settings
        , SharedModel
        )
import SubCmd exposing (SubCmd)
import Task
import Task.Parallel
import Temperature
import Ui
import Ui.Button
import Ui.Theme
import View exposing (View)



---- INIT ----


init : GenerationConfig -> ( World, SubCmd PlayingMsg Effect )
init generationConfig =
    ( emptyWorld
    , CreateGalaxy generationConfig
        |> Task.succeed
        |> Task.perform identity
        |> SubCmd.cmd
    )


getSolarSiblings : World -> EntityID -> Set EntityID
getSolarSiblings world child =
    case
        Logic.Component.get child world.parents
            |> Maybe.andThen
                (\solarSystemId ->
                    Maybe.map (Set.remove child)
                        (Logic.Component.get solarSystemId world.children)
                )
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
            Data.Orbit.distance orbitA
                |> Quantity.minus Length.astronomicalUnit
                |> Quantity.abs
                |> Length.inAstronomicalUnits

        distB : Float
        distB =
            Data.Orbit.distance orbitB
                |> Quantity.minus Length.astronomicalUnit
                |> Quantity.abs
                |> Length.inAstronomicalUnits
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
        CreateGalaxy generationConfig ->
            let
                ( generatedWorld, seed ) =
                    let
                        -- Filter out a civilization name if the player's chosen name matches
                        worldWithPlayerDataFilteredOut : World
                        worldWithPlayerDataFilteredOut =
                            case generationConfig.playerStuff of
                                Just playerStuff ->
                                    { emptyWorld
                                        | availableCivilizationNames =
                                            List.filter
                                                (\name ->
                                                    String.toLower (Data.Name.toString name) /= String.toLower (Data.Name.toString playerStuff.name)
                                                )
                                                emptyWorld.availableCivilizationNames
                                    }

                                Nothing ->
                                    emptyWorld
                    in
                    Random.step (generateGalaxy generationConfig worldWithPlayerDataFilteredOut) sharedModel.seed

                viableStartingPlanets : List ( EntityID, Orbit )
                viableStartingPlanets =
                    Set.toList generatedWorld.planets
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
                    Tuple.mapFirst
                        (\xs ->
                            Maybe.withDefault Dict.empty
                                (Maybe.map (\( planetId, _ ) -> Dict.singleton planetId (Population.millions 7))
                                    (List.head xs)
                                )
                        )
                        (Random.step (Random.List.shuffle viableStartingPlanets) seed)

                ( playerCiv, worldWithPlayerCivWithKnowledge ) =
                    case generationConfig.playerStuff of
                        Just playerStuff ->
                            let
                                ( playerCivId, worldWithPlayerCiv ) =
                                    Logic.Entity.Extra.create generatedWorld
                                        |> Logic.Entity.with ( Game.Components.civilizationPopulationSpec, inhabitedPlanets )
                                        |> Logic.Entity.with ( Game.Components.namedSpec, playerStuff.name )
                                        |> Logic.Entity.with ( Game.Components.reproductionSpec, playerStuff.reproductionMotivation )
                                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, Rate.fromFloat 0.1 )
                                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.map (\_ _ -> Percent.oneHundred) inhabitedPlanets )
                            in
                            ( playerCivId, worldWithPlayerCiv )
                                |> Logic.Entity.with
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
                                |> Tuple.mapFirst Just

                        Nothing ->
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
                                                    getSolarSiblings worldWithPlayerCivWithKnowledge planetId
                                            in
                                            List.concat
                                                [ ---- Local solar knowledge
                                                  List.map
                                                    (\siblingId ->
                                                        if Set.member siblingId worldWithPlayerCivWithKnowledge.stars then
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
                                                    (Set.toList (Set.diff worldWithPlayerCivWithKnowledge.stars solarSiblings))

                                                -- Knowledge of extra solar planets requires Optics and knowledge of at least 1 parent local star
                                                , List.map
                                                    (\nonSiblingPlanetId ->
                                                        ( KnowsOf nonSiblingPlanetId
                                                        , List.map
                                                            (\foreignStarId ->
                                                                Set.Any.fromList Data.Knowledge.comparableConfig
                                                                    [ Optics, KnowsOf foreignStarId ]
                                                            )
                                                            (Set.toList (Set.intersect worldWithPlayerCivWithKnowledge.stars (getSolarSiblings worldWithPlayerCivWithKnowledge nonSiblingPlanetId)))
                                                        )
                                                    )
                                                    (Set.toList (Set.diff worldWithPlayerCivWithKnowledge.planets solarSiblings))

                                                ---- Other Civ Knowledge
                                                ]
                                        )
                                        (Process.sleep (toFloat delay))
                                )
                                (Set.toList worldWithPlayerCivWithKnowledge.planets)
                        }
            in
            ( { worldWithPlayerCivWithKnowledge
                | playerCiv = playerCiv
                , playType =
                    case generationConfig.playerStuff of
                        Nothing ->
                            Observation

                        Just _ ->
                            Participation
                , civilizations =
                    case playerCiv of
                        Nothing ->
                            worldWithPlayerCivWithKnowledge.civilizations

                        Just civId ->
                            Set.insert civId worldWithPlayerCivWithKnowledge.civilizations
                , zoom =
                    Dict.keys (Logic.Component.toDict worldWithPlayerCivWithKnowledge.solarSystems)
                        |> List.filterMap
                            (\solarSystemId ->
                                Maybe.map
                                    (\galacticPosition ->
                                        Length.inMeters (Point3d.distanceFrom Point3d.origin galacticPosition)
                                    )
                                    (Logic.Component.get solarSystemId worldWithPlayerCivWithKnowledge.galaxyPositions)
                            )
                        |> List.sort
                        |> List.reverse
                        |> List.head
                        |> Maybe.map (\m -> m / 2)
                        |> Maybe.withDefault (25000 + (100 * 9460730000000000))
                , buildingKnowledgeState = buildingKnowledgeState
                , buildingKnowledge = Just ( 0, Set.size worldWithPlayerCivWithKnowledge.planets )
              }
            , SubCmd.batch
                [ SubCmd.effect (UpdateSeed finalSeed)
                , SubCmd.cmd parallelCmd
                ]
            )

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
                            Dict.keys (Logic.Component.toDict world.solarSystems)
                                |> List.filterMap
                                    (\solarSystemId ->
                                        Maybe.map
                                            (\galacticPosition ->
                                                Length.inMeters (Point3d.distanceFrom Point3d.origin galacticPosition)
                                            )
                                            (Logic.Component.get solarSystemId world.galaxyPositions)
                                    )
                                |> List.sort
                                |> List.reverse
                                |> List.head
                                |> Maybe.map (\m -> m / 2)
                                |> Maybe.withDefault (25000 + (100 * 9460730000000000))

                        FSolarSystem solarSystemId ->
                            -- a good number
                            Maybe.map
                                (\children ->
                                    let
                                        findLargest : List number -> number
                                        findLargest items =
                                            List.sort items
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault 0

                                        largestStarRadius : Float
                                        largestStarRadius =
                                            Set.toList (Set.intersect children world.stars)
                                                |> List.filterMap
                                                    (\starId ->
                                                        Maybe.map
                                                            (\starTemp ->
                                                                Point3d.fromMeters
                                                                    { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                                                    , y = 0
                                                                    , z = 0
                                                                    }
                                                                    |> Point3d.distanceFrom Point3d.origin
                                                                    |> Length.inMeters
                                                            )
                                                            (Logic.Component.get starId world.starTemperature)
                                                    )
                                                |> findLargest

                                        largestPlanetOrbit : Float
                                        largestPlanetOrbit =
                                            Set.toList (Set.intersect children world.planets)
                                                |> List.filterMap
                                                    (\planetId ->
                                                        Maybe.map
                                                            (\orbit -> Length.inMeters (Data.Orbit.distance orbit))
                                                            (Logic.Component.get planetId world.orbits)
                                                    )
                                                |> findLargest
                                    in
                                    max largestStarRadius largestPlanetOrbit
                                )
                                (Logic.Component.get solarSystemId world.children)
                                |> Maybe.withDefault 1196782965600

                        FStar starId ->
                            Logic.Component.get starId world.starTemperature
                                |> Maybe.map
                                    (\starTemp ->
                                        (Point3d.fromMeters
                                            { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                            , y = 0
                                            , z = 0
                                            }
                                            |> Point3d.distanceFrom Point3d.origin
                                        )
                                            |> Length.inMeters
                                    )
                                -- a good number
                                |> Maybe.withDefault 1196782965600

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
                updatedWorld
                    |> birthAndDeathSystem
                    |> (\w -> ( w, sharedModel.seed ))
                    |> discoverySystem Data.Knowledge.spec
                    |> expansionSystem
                    |> civilUnrestSystem
                    |> structureSystem
                    |> Tuple.mapSecond (\seed -> SubCmd.effect (UpdateSeed seed))

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
                                                    (Logic.Entity.Extra.create world
                                                        |> Logic.Entity.with
                                                            ( Data.Structure.civilizationStructuresSpec
                                                            , { creators = civId
                                                              , creationDate = world.starDate
                                                              , type_ = structureType
                                                              , planet = planetId
                                                              , name = name
                                                              }
                                                            )
                                                    )
                                                        |> Tuple.second
                                                )
                                                (Data.Structure.random personName)
                                        )
                                        (Data.Name.randomPlace nameSource)

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
                                        childIds
                                            |> Set.filter
                                                (\childId ->
                                                    (childId /= planetId) && Set.member childId world.planets
                                                )
                                            |> Set.toList
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
                    Logic.Entity.Extra.create world
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , civDetails.occupiedPlanets
                                |> Dict.get planetId
                                |> Maybe.withDefault (Population.millions 3)
                                |> Dict.singleton planetId
                            )
                        |> Logic.Entity.with
                            ( Game.Components.namedSpec
                            , Data.Name.fromString ("Renegade " ++ Data.Name.toString civDetails.name)
                            )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, civDetails.reproductionRate )
                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, civDetails.mortalityRate )
                        |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )

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
            percent
                |> Quantity.multiplyBy 100.0
                |> Quantity.divideBy (toFloat (List.length setList))
                |> Percent.toFloat
                |> floor
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
    -> NameSource
    -> ( ( Array (Maybe (AnySet String Knowledge)), Maybe Log ), Seed )
gainRandomKnowledge civKnowledge index allCivsKnowledge maybeCivKnowledge seed world civName civStyle nameSource =
    Random.step
        (Random.andThen
            (\gainsKnowledge ->
                if gainsKnowledge then
                    let
                        canBeLearned : List Knowledge
                        canBeLearned =
                            Set.Any.toList Data.Knowledge.comparableConfig
                                (Data.Knowledge.canBeLearned
                                    world.knowledgeTree
                                    civKnowledge
                                )
                    in
                    case canBeLearned of
                        [] ->
                            Random.constant ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )

                        first :: rest ->
                            Random.map2
                                (\personName knowledgeGained ->
                                    ( Array.set index
                                        (Just
                                            (Set.Any.insert Data.Knowledge.comparableConfig
                                                knowledgeGained
                                                civKnowledge
                                            )
                                        )
                                        allCivsKnowledge
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
                    ( solarSystemId, finalWorld )
                        |> Logic.Entity.with ( Game.Components.childrenSpec, Set.union planetIds starIds )
                        |> Logic.Entity.with ( Game.Components.positionSpec, galacticPosition )
                        |> Logic.Entity.with ( Game.Components.solarSystemSpec, SolarSystem )
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
            ( starId, world )
                |> Logic.Entity.with ( Data.Star.temperatureSpec, starTemperature )
                |> Logic.Entity.with ( Game.Components.parentSpec, solarSystemId )
                |> Tuple.mapSecond (\w -> { w | stars = Set.insert starId w.stars })
        )
        Data.Star.random


generatePlanet : EntityID -> ( EntityID, World ) -> Generator ( EntityID, World )
generatePlanet solarSystemId ( planetId, world ) =
    Random.andThen
        (\( planetType, waterPercent ) ->
            Random.map4
                (\orbitDistance orbitPeriod size updatedWorld ->
                    ( planetId, updatedWorld )
                        |> Logic.Entity.with ( Game.Components.planetTypeSpec, planetType )
                        |> Logic.Entity.with
                            ( Game.Components.orbitSpec
                            , Data.Orbit.create
                                { distance = Length.astronomicalUnits orbitDistance
                                , period = Data.EarthYear.earthYears orbitPeriod
                                }
                            )
                        |> Logic.Entity.with ( Game.Components.waterSpec, waterPercent )
                        |> Logic.Entity.with ( Game.Components.planetSizeSpec, size )
                        |> Logic.Entity.with ( Game.Components.parentSpec, solarSystemId )
                        |> Tuple.mapSecond (\w -> { w | planets = Set.insert planetId w.planets })
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


{-| TODO: Most of this should be moved to Data.Civilization module
-}
generateCivilization : World -> EntityID -> Name -> Generator World
generateCivilization worldWithFewerNames planetId name =
    Random.map
        (\initialPopulationSize reproductionRate mortalityRate initialHappiness civDensity coopVsComp nameSource senses descisionMakingStructure motivations ->
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
                        |> Logic.Entity.with ( Game.Components.civilizationDensitySpec, civDensity )
                        |> Logic.Entity.with
                            ( Game.Components.motivationsSpec
                            , motivations
                            )
                        |> Logic.Entity.with
                            ( Data.Civilization.styleSpec
                            , { cooperationVsCompetition = coopVsComp
                              , timeSinceLastMonument = worldWithFewerNames.starDate
                              , senses = senses
                              , descisionMakingStructure = descisionMakingStructure
                              }
                            )
                        |> Logic.Entity.with ( Game.Components.civilizationPersonNameSourceSpec, nameSource )

                ( _, worldWithNewCivWithKnowledge ) =
                    ( civId, worldWithNewCiv )
                        |> Logic.Entity.with
                            ( Data.Knowledge.spec
                            , Set.Any.fromList Data.Knowledge.comparableConfig
                                -- All civs know of themselves and their home planet
                                [ KnowsOf planetId
                                , KnowsOf civId
                                ]
                            )
            in
            { worldWithNewCivWithKnowledge | civilizations = Set.insert civId worldWithNewCivWithKnowledge.civilizations }
        )
        -- initialPopulationSize
        (Random.float 0.3 0.8)
        -- reproductionRate
        |> Random.Extra.andMap (Rate.random 0.2 0.3)
        -- mortalityRate
        |> Random.Extra.andMap (Rate.random 0.1 0.2)
        -- initialHappiness
        |> Random.Extra.andMap (Percent.random 0.9 1.0)
        -- civDensity: this needs to change based on entity size
        |> Random.Extra.andMap (Random.float 0.7 1.3)
        -- coopVsComp: should this be biased away from the extremes?
        |> Random.Extra.andMap (Random.float 0.0 1.0)
        |> Random.Extra.andMap Data.Name.randomNameSource
        |> Random.Extra.andMap
            -- Choose a random selection of 2-5 senses
            (Random.int 2 5
                |> Random.andThen
                    (\senseCount -> Random.List.choices senseCount Data.Civilization.allSenses)
                |> Random.map (\( senses, _ ) -> Set.Any.fromList Data.Civilization.senseComparableConfig senses)
            )
        -- descisionMakingStructure: should this be biased away from the extremes?
        |> Random.Extra.andMap (Random.float 0.0 1.0)
        |> Random.Extra.andMap
            (Random.map3
                (\expand explore dominate ->
                    Dict.fromList
                        [ ( "expand", expand )
                        , ( "explore", explore )
                        , ( "dominate", dominate )
                        ]
                )
                (Random.float -1.0 1.0)
                (Random.float -1.0 1.0)
                (Random.float -1.0 1.0)
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
                            playerCivKnowledge
                                |> Set.Any.toList Data.Knowledge.comparableConfig
                                |> List.filterMap
                                    (\knowledge ->
                                        case knowledge of
                                            KnowsOf id ->
                                                Just id

                                            _ ->
                                                Nothing
                                    )
                                |> Set.fromList

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


viewPlaying : SharedModel -> World -> Html PlayingMsg
viewPlaying sharedModel world =
    case world.buildingKnowledge of
        Just ( completed, total ) ->
            Ui.el [ Ui.height.fill ] <|
                Ui.column
                    [ Ui.justifySelf.center
                    , Ui.alignSelf.center
                    , Ui.width.shrink
                    , Ui.height.shrink
                    ]
                    [ Ui.text "Generating Galaxy"
                    , Html.progress
                        [ Html.Attributes.max (String.fromInt total)
                        , Html.Attributes.value (String.fromInt completed)
                        ]
                        []
                    ]

        Nothing ->
            Ui.stack []
                [ Ui.column
                    [ Ui.height.fill
                    ]
                    [ viewControls world
                    , (case world.viewStyle of
                        ThreeD ->
                            Ui.column

                        TwoD ->
                            Ui.row
                      )
                        [ Ui.height.fill

                        -- , scrollbarY
                        , Ui.padding.rem1
                        , Ui.gap.remHalf
                        ]
                        [ Ui.el
                            ((case world.viewStyle of
                                ThreeD ->
                                    []

                                TwoD ->
                                    [-- scrollbarY
                                    ]
                             )
                                ++ [ -- alignTop
                                     Ui.height.fill
                                   , Ui.borderStyle.solid
                                   , Ui.borderWidth.px1
                                   ]
                            )
                            (case world.spaceFocus of
                                FGalaxy ->
                                    viewGalaxy world

                                FSolarSystem id ->
                                    case Logic.Component.get id world.solarSystems of
                                        Just SolarSystem ->
                                            viewSlice
                                                [ Ui.Button.default [ Ui.width.shrink, Ui.height.shrink ]
                                                    { label = Ui.text "View Galaxy"
                                                    , onPress = Just (SetSpaceFocus FGalaxy)
                                                    }
                                                ]
                                                (viewSolarSystemDetailed sharedModel.settings world id)

                                        Nothing ->
                                            Ui.text "Missing solar system"

                                FStar starId ->
                                    if Set.member starId world.stars then
                                        viewSlice
                                            [ Ui.Button.default [ Ui.width.shrink, Ui.height.shrink ]
                                                { label = Ui.text "View Galaxy"
                                                , onPress = Just (SetSpaceFocus FGalaxy)
                                                }
                                            , Ui.Button.default [ Ui.width.shrink, Ui.height.shrink ]
                                                { label = Ui.text "View System"
                                                , onPress =
                                                    Maybe.map (\id -> SetSpaceFocus (FSolarSystem id))
                                                        (Logic.Component.get starId world.parents)
                                                }
                                            ]
                                            (viewStarDetailed world starId)

                                    else
                                        Ui.text "Missing star"

                                FPlanet planetId ->
                                    if Set.member planetId world.planets then
                                        viewSlice
                                            [ Ui.Button.default [ Ui.width.shrink, Ui.height.shrink ]
                                                { label = Ui.text "View Galaxy"
                                                , onPress = Just (SetSpaceFocus FGalaxy)
                                                }
                                            , Ui.Button.default [ Ui.width.shrink, Ui.height.shrink ]
                                                { label = Ui.text "View System"
                                                , onPress =
                                                    Maybe.map (\id -> SetSpaceFocus (FSolarSystem id))
                                                        (Logic.Component.get planetId world.parents)
                                                }
                                            ]
                                            (viewPlanetDetailed world planetId)

                                    else
                                        Ui.text "Missing planet"
                            )
                        , case world.civilizationFocus of
                            FAll ->
                                viewCivilizations world

                            FOne civId ->
                                viewCivilizationDetailed world civId
                        ]
                    ]
                , case world.settingsVisible of
                    Hidden ->
                        Ui.none

                    Visible ->
                        Shared.viewSettings sharedModel
                            |> Ui.map GotLocalSharedMessage
                            |> Ui.el
                                [ Ui.transform
                                    [ Ui.translate.down 64
                                    , Ui.translate.left 16
                                    ]
                                , Ui.width.shrink
                                , Ui.height.shrink
                                , Ui.justifySelf.end
                                ]
                ]


viewControls : World -> Html PlayingMsg
viewControls world =
    Ui.row
        [ Ui.padding.rem1
        , Ui.gap.rem1
        ]
        [ Ui.text "Game Speed:"
            |> Ui.el [ Ui.alignSelf.center ]
        , Ui.Button.toggle
            { label = Ui.text "||"
            , onPress = Just (SetTickRate Paused)
            , enabled = world.tickRate == Paused
            }
        , Ui.Button.toggle
            { label = Ui.text "|>"
            , onPress = Just (SetTickRate HalfSpeed)
            , enabled = world.tickRate == HalfSpeed
            }
        , Ui.Button.toggle
            { label = Ui.text ">"
            , onPress = Just (SetTickRate Normal)
            , enabled = world.tickRate == Normal
            }
        , Ui.Button.toggle
            { label = Ui.text ">>"
            , onPress = Just (SetTickRate Fast)
            , enabled = world.tickRate == Fast
            }
        , Ui.Button.toggle
            { label = Ui.text ">>>"
            , onPress = Just (SetTickRate ExtraFast)
            , enabled = world.tickRate == ExtraFast
            }
        , Ui.text (Data.EarthYear.formatAsStarDate world.starDate)
            |> Ui.el [ Ui.alignSelf.center ]
        , Ui.Button.default []
            (case world.viewStyle of
                ThreeD ->
                    { label = Ui.text "View 2D Galaxy"
                    , onPress = Just (GotViewStyle TwoD)
                    }

                TwoD ->
                    { label = Ui.text "View 3D Galaxy"
                    , onPress = Just (GotViewStyle ThreeD)
                    }
            )
        , Ui.el [] Ui.none
        , Ui.Button.negative
            { label = Ui.text "Delete Galaxy"
            , onPress = Just DeleteGalaxy
            }
        , Ui.el [] Ui.none
        , Ui.el
            [-- alignRight
            ]
            (Ui.Button.default []
                { label = Ui.text "⚙"
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


viewSlice : List (Html PlayingMsg) -> Html PlayingMsg -> Html PlayingMsg
viewSlice menuItems slice =
    Ui.stack
        [ Ui.height.fill
        ]
        [ slice
        , Ui.row
            [ Ui.padding.remHalf
            , Ui.gap.remHalf
            , Ui.width.shrink
            , Ui.height.shrink
            ]
            menuItems
        ]


viewGalaxy : World -> Html PlayingMsg
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


viewSolarSystemDetailed : Settings -> World -> EntityID -> Html PlayingMsg
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


viewStarDetailed : World -> EntityID -> Html PlayingMsg
viewStarDetailed model starId =
    case Logic.Component.get starId model.starTemperature of
        Nothing ->
            Ui.text "Your star is missing!"

        Just temp ->
            Ui.column
                [ Ui.gap.remHalf

                -- , paddingEach
                --     { top = 64
                --     , left = 8
                --     , bottom = 8
                --     , right = 8
                --     }
                ]
                [ Ui.text ("Name: S_" ++ String.fromInt starId)
                , Ui.text ("Temperature: " ++ String.fromInt (round (Temperature.inKelvins temp)) ++ "K")
                ]


viewPlanetDetailed : World -> EntityID -> Html PlayingMsg
viewPlanetDetailed world planetId =
    case Logic.Component.get planetId world.planetTypes of
        Nothing ->
            Ui.text "Your planet is missing!"

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
            Ui.column
                [ Ui.gap.remHalf
                , Ui.padding.each
                    { top = 64
                    , left = 8
                    , bottom = 8
                    , right = 8
                    }
                ]
                [ Ui.text ("Name: P_" ++ String.fromInt planetId)
                    |> Ui.el []
                , Ui.text
                    ("Terrain: "
                        ++ (case planetType of
                                Rocky ->
                                    "Rocky"

                                Gas ->
                                    "Gas"
                           )
                    )
                    |> Ui.el []
                , Ui.text "Civs on Planet:"
                    |> Ui.el []
                , case civsOnPlanet of
                    [] ->
                        Ui.text "None"

                    civs ->
                        Ui.column [ Ui.gap.remQuarter ]
                            (List.map
                                (\( civId, name ) ->
                                    Ui.Button.default [ Ui.width.shrink ]
                                        { label = Ui.text (Data.Name.toString name)
                                        , onPress = Just (SetCivilizationFocus (FOne civId))
                                        }
                                )
                                civs
                            )
                , Ui.text "Structures:"
                , Ui.column []
                    (List.filterMap
                        (\( _, structure ) ->
                            if structure.planet == planetId then
                                Just (Ui.text (Data.Structure.toString structure))

                            else
                                Nothing
                        )
                        (Logic.Component.toList world.civilizationStructures)
                    )
                ]


viewCivilizations : World -> Html PlayingMsg
viewCivilizations world =
    Ui.column
        [ Ui.gap.remHalf
        , Ui.height.shrink
        ]
        (List.map (viewCivilizationSimple world) (Set.toList world.civilizations))


viewCivilizationSimple : World -> EntityID -> Html PlayingMsg
viewCivilizationSimple world civId =
    case Logic.Component.get civId world.named of
        Nothing ->
            Ui.text "Civilization is missing"

        Just name ->
            Ui.row
                [ Ui.gap.remHalf
                , Ui.backgroundColor Ui.Theme.nearlyWhite
                ]
                [ Ui.Button.inspect
                    (Just (SetCivilizationFocus (FOne civId)))
                    |> Ui.el [ Ui.width.shrink ]
                , Ui.text (Data.Name.toString name)
                ]


viewCivilizationDetailed : World -> EntityID -> Html PlayingMsg
viewCivilizationDetailed world civId =
    Ui.column
        [ Ui.padding.rem1
        , Ui.height.fill
        , Ui.gap.rem1
        , Ui.borderStyle.solid
        , Ui.borderWidth.px2
        , Ui.borderColor Ui.Theme.darkGray
        , Ui.backgroundColor Ui.Theme.nearlyWhite
        ]
        (case getCivilizationDetails world civId of
            Nothing ->
                [ Ui.text "Civ is missing" ]

            Just details ->
                let
                    totalPopulationSize : Population
                    totalPopulationSize =
                        List.foldl (\( _, planetPupulationCount ) -> Population.plus planetPupulationCount)
                            (Population.millions 0)
                            (Dict.toList details.occupiedPlanets)
                in
                [ Ui.Button.default [ Ui.width.shrink ]
                    { label = Ui.text "Back"
                    , onPress = Just (SetCivilizationFocus FAll)
                    }
                , Ui.text ("The " ++ Data.Name.toString (Data.Name.plurualize details.name) ++ " have " ++ populationToString totalPopulationSize ++ " citizens.")
                , Ui.text ("Happiness " ++ happinessToString (averageCivilizationHappiness details.happiness))
                , case Logic.Component.get civId world.civilizationStyle of
                    Nothing ->
                        Ui.none

                    Just civStyle ->
                        Ui.column
                            [ Ui.gap.remQuarter
                            ]
                            [ Ui.row [ Ui.gap.rem3 ]
                                [ Ui.text "Cooperative"
                                , Ui.el
                                    [-- centerX
                                    ]
                                    (Ui.text "or")
                                , Ui.el
                                    [-- alignRight
                                    ]
                                    (Ui.text "Competitive")
                                ]
                            , Ui.el
                                [ Ui.borderWidth.px1

                                -- , inFront
                                --     (Ui.el
                                --         [ height (px 16)
                                --         , Border.width 2
                                --         , moveUp 8
                                --         , moveRight (civStyle.cooperationVsCompetition * 400)
                                --         ]
                                --         Ui.none
                                --     )
                                ]
                                Ui.none
                            ]
                , Ui.text "They occupy planets:"
                , Ui.column [ Ui.width.shrink, Ui.height.shrink ]
                    (List.map
                        (\( planetId, populationCount ) ->
                            Ui.row
                                [ Ui.gap.remHalf, Ui.height.shrink ]
                                [ Ui.Button.default
                                    [ Ui.width.shrink
                                    , Ui.height.shrink
                                    ]
                                    { label = Ui.text ("P_" ++ String.fromInt planetId)
                                    , onPress = Just (SetSpaceFocus (FPlanet planetId))
                                    }
                                , Ui.column [ Ui.gap.remQuarter, Ui.height.shrink ]
                                    [ Ui.text ("Population: " ++ populationToString populationCount)
                                        |> Ui.el [ Ui.height.shrink ]
                                    , Ui.el [ Ui.height.shrink ] <|
                                        case Dict.get planetId details.happiness of
                                            Just happiness ->
                                                Ui.text ("Happiness: " ++ happinessToString happiness)

                                            Nothing ->
                                                Ui.none
                                    ]
                                ]
                        )
                        (Dict.toList details.occupiedPlanets)
                    )
                , Ui.text "Logs"
                , Ui.column [ Ui.gap.remQuarter ] (List.map viewLog details.logs)
                ]
        )


averageCivilizationHappiness : Dict EntityID (Percent Happiness) -> Percent Happiness
averageCivilizationHappiness happiness =
    Quantity.divideBy (toFloat (Dict.size happiness))
        (List.foldl
            (\( _, happinessPerPlanet ) ->
                Quantity.plus happinessPerPlanet
            )
            Percent.zero
            (Dict.toList happiness)
        )


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


viewLog : Log -> Html PlayingMsg
viewLog log =
    Ui.column
        [ Ui.borderStyle.solid
        , Ui.borderWidth.px1
        , Ui.padding.remQuarter
        ]
        [ Ui.text (Data.EarthYear.formatAsStarDate log.time)
        , Ui.paragraph [] [ Ui.text log.description ]
        ]


happinessToString : Percent Happiness -> String
happinessToString happiness =
    let
        hap : Float
        hap =
            Percent.toFloat happiness
    in
    if hap > 1.2 then
        ""

    else if hap > 1.0 then
        "🙂"

    else if hap == 1.0 then
        "😐"

    else if hap < 0.8 then
        "😠"

    else
        "☹️"


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
