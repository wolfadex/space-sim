module Galaxy2d exposing (viewGalaxy, viewSolarSystem)

import Data.Name
import Data.Orbit
import Dict
import Game.Components exposing (World)
import Html exposing (Html)
import Length
import Logic.Component
import Logic.Entity exposing (EntityID)
import Set exposing (Set)
import Ui
import Ui.Button
import Ui.Theme


viewGalaxy :
    { onPressSolarSystem : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> Html msg
viewGalaxy onPress world =
    Ui.column
        [ Ui.gap.remHalf
        , Ui.backgroundColor Ui.Theme.darkGray
        ]
        (List.map (viewSolarSystemSimple onPress world) (Dict.keys (Logic.Component.toDict world.solarSystems)))


viewSolarSystemSimple :
    { onPressSolarSystem : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> EntityID
    -> Html msg
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
    Ui.column
        [ Ui.gap.remHalf
        , Ui.backgroundColor Ui.Theme.nearlyWhite
        ]
        [ Ui.row
            [ Ui.gap.remHalf
            ]
            [ Ui.text ("Solar System: SS_" ++ String.fromInt solarSystemId)
                |> Ui.el []
            , Ui.Button.inspect
                (Just (onPressSolarSystem solarSystemId))
                |> Ui.el [ Ui.width.shrink ]
            ]
        , Ui.text ("Stars: " ++ String.fromInt starCount)
        , Ui.text ("Planets: " ++ String.fromInt planetCount)
        , Ui.rowWrapped [ Ui.gap.remHalf ]
            (Ui.text "Occupied by: "
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
                                            Maybe.withDefault ("CIV_" ++ String.fromInt civId) (Maybe.map Data.Name.toString (Logic.Component.get civId world.named))
                                    in
                                    Just
                                        (if Just civId == focusedCivilization then
                                            Ui.Button.primary []
                                                { label = Ui.text civName
                                                , onPress = Just (onPressCivilization civId)
                                                }

                                         else
                                            Ui.Button.default []
                                                { label = Ui.text civName
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
    -> Html msg
viewSolarSystem { onPressPlanet, onPressStar, onPressCivilization, focusedCivilization } solarSystemId stars world planets =
    Ui.column
        [-- paddingEach
         --     { top = 64
         --     , left = 8
         --     , bottom = 8
         --     , right = 8
         --     }
        ]
        [ Ui.text ("Solar System: SS_" ++ String.fromInt solarSystemId)
        , Ui.column [ Ui.padding.remHalf ]
            [ Ui.text "Stars:"
            , Ui.column [ Ui.padding.remHalf, Ui.gap.remQuarter ]
                (List.map (viewStarSimple onPressStar world)
                    (Set.toList stars)
                )
            ]
        , Ui.column [ Ui.padding.remHalf ]
            [ Ui.text "Planets:"
            , Ui.column [ Ui.padding.remHalf, Ui.gap.remQuarter ]
                (List.map
                    (\( planetId, _ ) ->
                        viewPlanetSimple
                            { onPressPlanet = onPressPlanet
                            , onPressCivilization = onPressCivilization
                            , focusedCivilization = focusedCivilization
                            }
                            world
                            planetId
                    )
                    (List.sortBy (\( _, orbit ) -> Length.inAstronomicalUnits (Data.Orbit.distance orbit))
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


viewStarSimple : (EntityID -> msg) -> World -> EntityID -> Html msg
viewStarSimple onPress _ starId =
    Ui.row
        [ Ui.gap.remHalf ]
        [ Ui.el [] (Ui.text ("S_" ++ String.fromInt starId))
        , Ui.Button.inspect (Just (onPress starId))
        ]


viewPlanetSimple :
    { onPressPlanet : EntityID -> msg
    , onPressCivilization : EntityID -> msg
    , focusedCivilization : Maybe EntityID
    }
    -> World
    -> EntityID
    -> Html msg
viewPlanetSimple { onPressPlanet, onPressCivilization, focusedCivilization } world planetId =
    Ui.column
        [ Ui.gap.remHalf ]
        [ Ui.row
            [ Ui.gap.remHalf ]
            [ Ui.el [] (Ui.text ("P_" ++ String.fromInt planetId))
            , Ui.Button.inspect (Just (onPressPlanet planetId))
            ]
        , Ui.rowWrapped [ Ui.gap.remHalf ]
            (List.filterMap
                (\civId ->
                    Maybe.andThen
                        (\dictPlanetPopulatiopns ->
                            if Dict.member planetId dictPlanetPopulatiopns then
                                let
                                    civName : String
                                    civName =
                                        Maybe.withDefault ("CIV_" ++ String.fromInt civId)
                                            (Maybe.map Data.Name.toString (Logic.Component.get civId world.named))
                                in
                                Just
                                    (if Just civId == focusedCivilization then
                                        Ui.Button.primary []
                                            { label = Ui.text civName
                                            , onPress = Just (onPressCivilization civId)
                                            }

                                     else
                                        Ui.Button.default []
                                            { label = Ui.text civName
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
