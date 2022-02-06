module Playing exposing
    ( Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Browser.Dom exposing (Viewport)
import Browser.Events
import Data.Knowledge exposing (Knowledge(..))
import Data.Names exposing (CivilizationName)
import Data.Star
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
        , Orbit
        , Reproduction
        , SpaceFocus(..)
        , TickRate(..)
        , ViewStyle(..)
        , Visible(..)
        , World
        , emptyWorld
        )
import Json.Decode exposing (Value)
import Length exposing (Meters)
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
import Shared
    exposing
        ( Effect(..)
        , PlayType(..)
        , Settings
        , SharedModel
        , SharedMsg
        )
import SubCmd exposing (SubCmd)
import Svg.Attributes exposing (r)
import Temperature
import Ui.Button
import Ui.Theme
import View exposing (View)



---- INIT ----


init : SharedModel -> PlayType -> ( World, SubCmd Msg Effect )
init sharedModel playType =
    let
        ( generatedWorld, seed ) =
            let
                generationArguments : { minSolarSystemsToGenerate : Int, maxSolarSystemsToGenerate : Int }
                generationArguments =
                    case playType of
                        Participation r ->
                            { minSolarSystemsToGenerate = r.minSolarSystemsToGenerate
                            , maxSolarSystemsToGenerate = r.maxSolarSystemsToGenerate
                            }

                        Observation r ->
                            r

                -- Filter out a civilization name if the player's chosen name matches
                worldWithPlayerDataFilteredOut : World
                worldWithPlayerDataFilteredOut =
                    case playType of
                        Participation r ->
                            { emptyWorld
                                | availableCivilizationNames =
                                    List.filter (\name -> String.toLower name.singular /= String.toLower r.name.singular)
                                        emptyWorld.availableCivilizationNames
                            }

                        Observation _ ->
                            emptyWorld
            in
            Random.step (generateGalaxy generationArguments worldWithPlayerDataFilteredOut) sharedModel.seed

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
            case playType of
                Participation r ->
                    Logic.Entity.Extra.create generatedWorld
                        |> Logic.Entity.with ( Game.Components.civilizationPopulationSpec, inhabitedPlanets )
                        |> Logic.Entity.with ( Game.Components.namedSpec, r.name )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, Rate.fromFloat 0.3 )
                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, Rate.fromFloat 0.1 )
                        |> Logic.Entity.with
                            ( Game.Components.civilizationHappinessSpec
                            , Dict.map (\_ _ -> Percent.fromFloat 100.0) inhabitedPlanets
                            )
                        |> Logic.Entity.with ( Data.Knowledge.spec, Set.Any.empty )

                Observation _ ->
                    ( -1, generatedWorld )
    in
    ( { worldWithPlayerCiv
        | playerCiv = playerCiv
        , civilizations = Set.insert playerCiv worldWithPlayerCiv.civilizations
        , zoom =
            Set.toList worldWithPlayerCiv.solarSystems
                |> List.filterMap
                    (\solarSystemId ->
                        Maybe.map
                            (\galacticPosition ->
                                Length.inMeters (Point3d.distanceFrom Point3d.origin galacticPosition)
                            )
                            (Logic.Component.get solarSystemId worldWithPlayerCiv.galaxyPositions)
                    )
                |> List.sort
                |> List.reverse
                |> List.head
                |> Maybe.map (\m -> m / 2)
                |> Maybe.withDefault (25000 + (100 * 9460730000000000))
        , knowledgeTree =
            Data.Knowledge.buildKnowledgeTree
                (List.concatMap
                    (\planetId ->
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
                    (Set.toList worldWithPlayerCiv.planets)
                )
      }
    , SubCmd.batch
        [ Galaxy3d.getGalaxyViewport GotGalaxyViewport
        , SubCmd.effect (UpdateSeed finalSeed)
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
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]


type Msg
    = DeleteGalaxy
    | SetSpaceFocus SpaceFocus
    | SetCivilizationFocus CivilizationFocus
    | Tick Float
    | SetTickRate TickRate
    | GotViewStyle ViewStyle
    | WindowResized
    | GotGalaxyViewport (Result Browser.Dom.Error Viewport)
    | GotZoom Value
    | GotZoomChange Float
    | GotRotationChange Float
    | GotSettingsVisible Visible
    | GotLocalSharedMessage SharedMsg


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


update : SharedModel -> Msg -> World -> ( World, SubCmd Msg Effect )
update sharedModel msg world =
    case msg of
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
                            Set.toList world.solarSystems
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
                            Maybe.map
                                (\children ->
                                    let
                                        largestStarRadius : Float
                                        largestStarRadius =
                                            Set.intersect children world.stars
                                                |> Set.toList
                                                |> List.filterMap
                                                    (\starId ->
                                                        Maybe.map
                                                            (\starTemp ->
                                                                Point3d.distanceFrom Point3d.origin
                                                                    (Point3d.fromMeters
                                                                        { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                                                        , y = 0
                                                                        , z = 0
                                                                        }
                                                                    )
                                                                    |> Length.inMeters
                                                            )
                                                            (Logic.Component.get starId world.starTemperature)
                                                    )
                                                |> List.sort
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault 0

                                        largestPlanetOrbit : Float
                                        largestPlanetOrbit =
                                            Set.intersect children world.planets
                                                |> Set.toList
                                                |> List.filterMap
                                                    (\planetId ->
                                                        Maybe.map
                                                            (\orbit ->
                                                                Length.inMeters (Quantity.multiplyBy (toFloat orbit) Length.astronomicalUnit)
                                                            )
                                                            (Logic.Component.get planetId world.orbits)
                                                    )
                                                |> List.sort
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault 0
                                    in
                                    max largestStarRadius largestPlanetOrbit
                                )
                                (Logic.Component.get solarSystemId world.children)
                                -- a good number
                                |> Maybe.withDefault 1196782965600

                        FStar starId ->
                            Maybe.map
                                (\starTemp ->
                                    Point3d.distanceFrom Point3d.origin
                                        (Point3d.fromMeters
                                            { x = Length.inMeters (Data.Star.temperatureToRadius starTemp)
                                            , y = 0
                                            , z = 0
                                            }
                                        )
                                        |> Length.inMeters
                                )
                                (Logic.Component.get starId world.starTemperature)
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
                                ( True, remaining - baseTickTime * 2, world.starDate + 1 )

                            else
                                ( False, remaining, world.starDate )

                        Normal ->
                            if remaining - baseTickTime >= 0 then
                                ( True, remaining - baseTickTime, world.starDate + 1 )

                            else
                                ( False, remaining, world.starDate )

                        Fast ->
                            if remaining - baseTickTime / 4 >= 0 then
                                ( True, remaining - baseTickTime / 4, world.starDate + 1 )

                            else
                                ( False, remaining, world.starDate )

                        ExtraFast ->
                            if remaining - baseTickTime / 8 >= 0 then
                                ( True, remaining - baseTickTime / 8, world.starDate + 1 )

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
                        Game.Components.civilizationReproductionRateSpec
                        Game.Components.civilizationMortalityRateSpec
                        Game.Components.civilizationPopulationSpec
                    |> (\w -> ( w, sharedModel.seed ))
                    |> discoverySystem Data.Knowledge.spec
                    |> expansionSystem
                    |> civilUnrestSystem
                    |> Tuple.mapSecond (UpdateSeed >> SubCmd.effect)

            else
                ( updatedWorld
                , SubCmd.none
                )


setZoom : World -> Float -> ( World, SubCmd Msg Effect )
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
            Set.toList world.civilizations
                |> List.filterMap
                    (\civId ->
                        Maybe.map2
                            (\knowledge population -> { id = civId, knowledge = knowledge, populatedPlanets = Dict.keys population })
                            (Logic.Component.get civId world.civilizationKnowledge)
                            (Logic.Component.get civId world.civilizationPopulations)
                    )
    in
    List.foldl
        (\civ ( nextWorld, nextSeed ) ->
            let
                totalPopulationSize : Population
                totalPopulationSize =
                    Logic.Component.get civ.id world.civilizationPopulations
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.foldl (\( _, planetPupulationCount ) -> Population.plus planetPupulationCount) (Population.millions 0)
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
                                        Set.filter
                                            (\childId ->
                                                (childId /= planetId) && Set.member childId world.planets
                                            )
                                            childIds
                                            |> Set.toList
                                    )
                                    (Logic.Component.get solarSystemId world.children)
                            )
                            (Logic.Component.get planetId world.parents)

                    allPopulatedPlanets : Set EntityID
                    allPopulatedPlanets =
                        Logic.Component.toList world.civilizationPopulations
                            |> List.concatMap (Tuple.second >> Dict.keys)
                            |> Set.fromList

                    allAvailablePlanets : Set EntityID
                    allAvailablePlanets =
                        world.planets
                            |> Set.filter
                                (\planetId ->
                                    case Logic.Component.get planetId world.planetTypes of
                                        Nothing ->
                                            False

                                        Just Gas ->
                                            False

                                        Just Rocky ->
                                            True
                                )
                            |> (\allPlanets -> Set.diff allPlanets allPopulatedPlanets)

                    planetsInSameSolarSystem : List EntityID
                    planetsInSameSolarSystem =
                        List.concat (List.filterMap filterBySameSolarSystem civ.populatedPlanets)

                    possiblePlanetsToExpandInto : List ( Float, EntityID )
                    possiblePlanetsToExpandInto =
                        if Data.Knowledge.knows civ.knowledge FTLSpaceTravel then
                            Set.toList allAvailablePlanets
                                |> List.map
                                    (\planetId ->
                                        ( if List.member planetId planetsInSameSolarSystem then
                                            2.0

                                          else
                                            1.0
                                        , planetId
                                        )
                                    )

                        else if Data.Knowledge.knows civ.knowledge InterplanetarySpaceTravel then
                            planetsInSameSolarSystem
                                |> List.filterMap
                                    (\planetId ->
                                        if Set.member planetId allAvailablePlanets then
                                            Just ( 1.0, planetId )

                                        else
                                            Nothing
                                    )

                        else
                            []
                in
                case possiblePlanetsToExpandInto of
                    [] ->
                        ( nextWorld, nextSeed )

                    _ ->
                        let
                            ( expandedWorld, seed ) =
                                Random.step (possiblyExpandToPlanet civ.id possiblePlanetsToExpandInto world) nextSeed
                        in
                        ( expandedWorld, seed )
        )
        ( world, initialSeed )
        planetsAndKnowledge


possiblyExpandToPlanet : EntityID -> List ( Float, EntityID ) -> World -> Generator World
possiblyExpandToPlanet civId possiblePlanetsToExpandInto world =
    weightedChoose possiblePlanetsToExpandInto
        |> Random.map
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
            Random.step (generateRevoltingCivs revoltingCivs world) initialSeed
    in
    ( worldWithNewCivs, seed )


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

                ( _, worldWithNewCivWithKnowledge ) =
                    ( civId, worldWithNewCiv )
                        |> Logic.Entity.with
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
        (Percent.random 95.0 100.0)
        (Random.andThen
            (\percent -> dropRandom Data.Knowledge.comparableConfig percent civDetails.knowledge)
            (Percent.random 5.0 25.0)
        )
        (Random.andThen
            (\percent -> dropRandom Data.Knowledge.comparableConfig percent civDetails.knowledge)
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
            let
                civName : CivilizationName
                civName =
                    case Array.get index world.named of
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
                        world
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
    -> World
    -> CivilizationName
    -> ( ( Array (Maybe (AnySet String Knowledge)), Maybe Log ), Seed )
gainRandomKnowledge civKnowledge index allCivsKnowledge maybeCivKnowledge seed world civName =
    Random.step
        (Random.andThen
            (\gainsKnowledge ->
                if gainsKnowledge then
                    let
                        canBeLearned : List Knowledge
                        canBeLearned =
                            Data.Knowledge.canBeLearned world.knowledgeTree civKnowledge
                                |> Set.Any.toList Data.Knowledge.comparableConfig
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
                                        , description = Data.Names.enhancedEventDescription civName personName ++ " gained new knowledge."
                                        , civilizationId = index
                                        }
                                    )
                                )
                                Data.Names.randomPerson
                                (Random.uniform first rest)

                else
                    Random.constant ( Array.set index maybeCivKnowledge allCivsKnowledge, Nothing )
            )
            (Random.Extra.oneIn 100)
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


generateGalaxy : { r | minSolarSystemsToGenerate : Int, maxSolarSystemsToGenerate : Int } -> World -> Generator World
generateGalaxy { minSolarSystemsToGenerate, maxSolarSystemsToGenerate } model =
    generateManyEntities minSolarSystemsToGenerate maxSolarSystemsToGenerate model generateSolarSystem
        |> Random.map Tuple.second


generateSolarSystem : ( EntityID, World ) -> Generator ( EntityID, World )
generateSolarSystem ( solarSystemId, world ) =
    Random.weighted ( 56.0, 1 ) [ ( 33.0, 2 ), ( 8.0, 3 ), ( 1.0, 4 ), ( 1.0, 5 ), ( 1.0, 6 ), ( 1.0, 7 ) ]
        |> Random.andThen (\starCount -> generateManyEntities starCount starCount world (generateStar solarSystemId))
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
    Random.map2 Tuple.pair
        (Random.uniform Rocky [ Gas ])
        (Percent.random 0.0 100.0)
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
        (\initialPopulationSize reproductionRate mortalityRate _ ->
            let
                ( civId, worldWithNewCiv ) =
                    Logic.Entity.Extra.create worldWithFewerNames
                        |> Logic.Entity.with
                            ( Game.Components.civilizationPopulationSpec
                            , Dict.singleton planetId (Population.millions initialPopulationSize)
                            )
                        |> Logic.Entity.with ( Game.Components.namedSpec, name )
                        |> Logic.Entity.with ( Game.Components.civilizationReproductionRateSpec, reproductionRate )
                        -- |> Logic.Entity.with ( Game.Components.civilizationHappinessSpec, Dict.singleton planetId initialHappiness )
                        |> Logic.Entity.with ( Game.Components.civilizationMortalityRateSpec, mortalityRate )

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
            { worldWithNewCiv | civilizations = Set.insert civId worldWithNewCivWithKnowledge.civilizations }
        )
        (Random.float 3 10)
        (Rate.random 0.002 0.003)
        (Rate.random 0.1 0.2)
        (Percent.random 90.0 100.0)


generateCivilizationName : World -> Generator ( Maybe CivilizationName, World )
generateCivilizationName world =
    Random.List.choose world.availableCivilizationNames
        |> Random.map
            (\( chosenName, remainingNames ) ->
                ( chosenName, { world | availableCivilizationNames = remainingNames } )
            )


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


view : SharedModel -> World -> View Msg
view sharedModel world =
    { title = "Hello Space!"
    , body = viewPlaying sharedModel world
    }


viewPlaying : SharedModel -> World -> Element Msg
viewPlaying sharedModel world =
    column
        [ width fill
        , height fill
        , inFront <|
            case world.settingsVisible of
                Hidden ->
                    none

                Visible ->
                    map GotLocalSharedMessage (Shared.viewSettings sharedModel.settings)
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
                            viewSlice
                                [ Ui.Button.default
                                    { label = text "View Galaxy"
                                    , onPress = Just (SetSpaceFocus FGalaxy)
                                    }
                                ]
                                (viewSolarSystemDetailed sharedModel.settings world id)

                        else
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
                                        Maybe.map (FSolarSystem >> SetSpaceFocus)
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
                                        Maybe.map (FSolarSystem >> SetSpaceFocus)
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


viewControls : World -> Element Msg
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
        , el [ alignRight ]
            (Ui.Button.default
                { label = text "⚙"
                , onPress =
                    Just <|
                        case world.settingsVisible of
                            Visible ->
                                GotSettingsVisible Hidden

                            Hidden ->
                                GotSettingsVisible Visible
                }
            )
        ]


viewSlice : List (Element Msg) -> Element Msg -> Element Msg
viewSlice menuItems slice =
    el
        [ height fill
        , width fill
        , inFront <|
            row [ padding 8, spacing 8 ] menuItems
        ]
        slice


viewGalaxy : World -> Element Msg
viewGalaxy world =
    case world.viewStyle of
        ThreeD ->
            Galaxy3d.viewGalaxy
                { onPressSolarSystem = FSolarSystem >> SetSpaceFocus
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
                { onPressSolarSystem = FSolarSystem >> SetSpaceFocus
                , onPressCivilization = FOne >> SetCivilizationFocus
                , focusedCivilization =
                    case world.civilizationFocus of
                        FAll ->
                            Nothing

                        FOne id ->
                            Just id
                }
                world


viewSolarSystemDetailed : Settings -> World -> EntityID -> Element Msg
viewSolarSystemDetailed settings world solarSystemId =
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
                { onPressStar = Just (FStar >> SetSpaceFocus)
                , onPressPlanet = Just (FPlanet >> SetSpaceFocus)
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
                { onPressPlanet = FPlanet >> SetSpaceFocus
                , onPressStar = FStar >> SetSpaceFocus
                , onPressCivilization = FOne >> SetCivilizationFocus
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


viewStarDetailed : World -> EntityID -> Element Msg
viewStarDetailed model starId =
    case Logic.Component.get starId model.starTemperature of
        Nothing ->
            text "Your star is missing!"

        Just temp ->
            column
                [ spacing 8 ]
                [ text ("Name: S_" ++ String.fromInt starId)
                , text ("Size: " ++ String.fromFloat (Temperature.inKelvins temp) ++ "K")
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
            , height fill
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
                , Background.color Ui.Theme.nearlyWhite
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
        , Background.color Ui.Theme.nearlyWhite
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
