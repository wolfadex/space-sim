module Galaxy2d exposing (viewGalaxy, viewSolarSystem)

import Dict
import Element exposing (..)
import Element.Background as Background
import Game.Components exposing (World)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Set exposing (Set)
import Ui.Button
import Ui.Theme


viewGalaxy :
    { onPressSolarSystem : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> Element msg
viewGalaxy onPress world =
    column
        [ spacing 8
        , width fill
        , spacing 8
        , Background.color Ui.Theme.darkGray
        ]
        (List.map (viewSolarSystemSimple onPress world) (Set.toList world.solarSystems))


viewSolarSystemSimple :
    { onPressSolarSystem : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> EntityID
    -> Element msg
viewSolarSystemSimple { onPressSolarSystem, onPressCivilization, focusedCivilization } world solarSystemId =
    let
        ( starCount, planetCount ) =
            Tuple.mapBoth Set.size
                Set.size
                (Maybe.withDefault ( Set.empty, Set.empty )
                    (Maybe.map
                        (\children ->
                            ( Set.intersect children world.stars
                            , Set.intersect children world.planets
                            )
                        )
                        (Logic.Component.get solarSystemId world.children)
                    )
                )
    in
    column
        [ padding 8
        , Background.color Ui.Theme.nearlyWhite
        , width fill
        ]
        [ row
            [ spacing 8, width fill ]
            [ el [ width fill ] (text ("Solar System: SS_" ++ String.fromInt solarSystemId))
            , Ui.Button.inspect
                (Just (onPressSolarSystem solarSystemId))
            ]
        , text ("Stars: " ++ String.fromInt starCount)
        , text ("Planets: " ++ String.fromInt planetCount)
        , wrappedRow [ spacing 8 ]
            (text "Occupied by: "
                :: List.filterMap
                    (\civId ->
                        Maybe.andThen
                            (\dictPlanetPopulatiopns ->
                                let
                                    solarSystemsCivIsIn : List EntityID
                                    solarSystemsCivIsIn =
                                        List.filterMap
                                            (\planetId ->
                                                Logic.Component.get planetId world.parents
                                            )
                                            (Dict.keys dictPlanetPopulatiopns)
                                in
                                if List.any ((==) solarSystemId) solarSystemsCivIsIn then
                                    let
                                        civName : String
                                        civName =
                                            Maybe.withDefault ("CIV_" ++ String.fromInt civId) (Maybe.map .singular (Logic.Component.get civId world.named))
                                    in
                                    Just
                                        (if Just civId == focusedCivilization then
                                            Ui.Button.primary
                                                { label = text civName
                                                , onPress = Just (onPressCivilization civId)
                                                }

                                         else
                                            Ui.Button.default
                                                { label = text civName
                                                , onPress = Just (onPressCivilization civId)
                                                }
                                        )

                                else
                                    Nothing
                            )
                            (Logic.Component.get civId world.civilizationPopulations)
                    )
                    (Set.toList world.civilizations)
            )
        ]


viewSolarSystem :
    { onPressStar : EntityID -> msg
    , onPressPlanet : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> EntityID
    -> Set EntityID
    -> World
    -> Set EntityID
    -> Element msg
viewSolarSystem { onPressPlanet, onPressStar, onPressCivilization, focusedCivilization } solarSystemId stars world planets =
    column
        [ padding 8 ]
        [ text ("Solar System: SS_" ++ String.fromInt solarSystemId)
        , column [ padding 8 ]
            [ text "Stars:"
            , column [ padding 8, spacing 4 ]
                (List.map (viewStarSimple onPressStar world)
                    (Set.toList stars)
                )
            ]
        , column [ padding 8 ]
            [ text "Planets:"
            , column [ padding 8, spacing 4 ]
                (List.map
                    (Tuple.first
                        >> viewPlanetSimple
                            { onPressPlanet = onPressPlanet
                            , onPressCivilization = onPressCivilization
                            , focusedCivilization = focusedCivilization
                            }
                            world
                    )
                    (List.sortBy (\( _, orbit ) -> orbit)
                        (List.filterMap
                            (\planetId ->
                                Maybe.map
                                    (Tuple.pair planetId)
                                    (Logic.Component.get planetId world.orbits)
                            )
                            (Set.toList planets)
                        )
                    )
                )
            ]
        ]


viewStarSimple : (EntityID -> msg) -> World -> EntityID -> Element msg
viewStarSimple onPress _ starId =
    row
        [ spacing 8, width fill ]
        [ el [ width fill ] (text ("S_" ++ String.fromInt starId))
        , Ui.Button.inspect (Just (onPress starId))
        ]


viewPlanetSimple :
    { onPressPlanet : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> EntityID
    -> Element msg
viewPlanetSimple { onPressPlanet, onPressCivilization, focusedCivilization } world planetId =
    column
        [ spacing 8, width fill ]
        [ row
            [ spacing 8, width fill ]
            [ el [ width fill ] (text ("P_" ++ String.fromInt planetId))
            , Ui.Button.inspect (Just (onPressPlanet planetId))
            ]
        , wrappedRow [ spacing 8 ]
            (List.filterMap
                (\civId ->
                    Maybe.andThen
                        (\dictPlanetPopulatiopns ->
                            if Dict.member planetId dictPlanetPopulatiopns then
                                let
                                    civName : String
                                    civName =
                                        Maybe.withDefault ("CIV_" ++ String.fromInt civId)
                                            (Maybe.map .singular (Logic.Component.get civId world.named))
                                in
                                Just
                                    (if Just civId == focusedCivilization then
                                        Ui.Button.primary
                                            { label = text civName
                                            , onPress = Just (onPressCivilization civId)
                                            }

                                     else
                                        Ui.Button.default
                                            { label = text civName
                                            , onPress = Just (onPressCivilization civId)
                                            }
                                    )

                            else
                                Nothing
                        )
                        (Logic.Component.get civId world.civilizationPopulations)
                )
                (Set.toList world.civilizations)
            )
        ]
