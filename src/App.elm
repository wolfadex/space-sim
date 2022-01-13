module App exposing
    ( Focus(..)
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

import Effect exposing (Effect)
import Element exposing (..)
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Game.Components
    exposing
        ( CelestialBodyForm(..)
        , CivilizationReproductionRate
        , Name
        , Orbit
        , ScaledNumber(..)
        , StarSize(..)
        , Water
        )
import Logic.Component exposing (Spec)
import Logic.Entity exposing (EntityID)
import Logic.Entity.Extra
import Logic.System exposing (System)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Shared exposing (Flags)
import Time
import Ui.Button
import Ui.Text
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
    , focus : Focus
    , tickRate : TickRate

    -- ECS stuff
    , ecsInternals : Logic.Entity.Extra.Internals
    , civilizationSizes : Logic.Component.Set ScaledNumber
    , named : Logic.Component.Set Name
    , civilizationReproductionRates : Logic.Component.Set CivilizationReproductionRate
    , celestialBodyForms : Logic.Component.Set CelestialBodyForm
    , starForms : Logic.Component.Set StarSize
    , parents : Logic.Component.Set EntityID
    , children : Logic.Component.Set (Set EntityID)
    , orbits : Logic.Component.Set Orbit
    , waterContent : Logic.Component.Set Water
    , planetSize : Logic.Component.Set Float

    -- Book keeping for plants, stars, and their grouping (solar systems)
    , planets : Set EntityID
    , stars : Set EntityID
    , solarSystems : Set EntityID
    , playerCiv : EntityID
    }


type Focus
    = FGalaxy
    | FSolarSystem EntityID
    | FStar EntityID
    | FPlanet EntityID


type TickRate
    = Paused
    | Normal
    | Fast
    | ExtraFast


emptyWorld : World
emptyWorld =
    { seed = Random.initialSeed 0
    , focus = FGalaxy
    , tickRate = Normal
    , ecsInternals = Logic.Entity.Extra.initInternals
    , civilizationSizes = Logic.Component.empty
    , named = Logic.Component.empty
    , civilizationReproductionRates = Logic.Component.empty
    , celestialBodyForms = Logic.Component.empty
    , starForms = Logic.Component.empty
    , parents = Logic.Component.empty
    , children = Logic.Component.empty
    , orbits = Logic.Component.empty
    , waterContent = Logic.Component.empty
    , planetSize = Logic.Component.empty
    , planets = Set.empty
    , stars = Set.empty
    , solarSystems = Set.empty
    , playerCiv = -1
    }


init : Flags -> ( Model, Effect Msg )
init flags =
    ( NewGame
        { seed = Random.initialSeed flags.seed0
        , civilizationNameSingular = ""
        , civilizationNamePlural = ""
        , hasUniquePluralName = False
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
        -- 3 seconds
        baseTickTime : Float
        baseTickTime =
            3000
    in
    case tickRate of
        Paused ->
            1 / 0

        -- Infinity
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
    | SetFocus Focus
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

                ( playerCiv, worldWithPlayerCiv ) =
                    Logic.Entity.Extra.create generatedWorld
                        |> Logic.Entity.with ( Game.Components.civilizationSizeSpec, Millions 100 )
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
            in
            ( Playing
                { worldWithPlayerCiv
                    | playerCiv = playerCiv
                    , seed = seed
                }
            , Effect.none
            )


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
            ( Playing { world | focus = focus }, Effect.none )

        Tick ->
            ( Playing (birthSystem Game.Components.civilizationReproductionRateSpec Game.Components.civilizationSizeSpec world)
            , Effect.none
            )


birthSystem : Spec CivilizationReproductionRate world -> Spec ScaledNumber world -> System world
birthSystem =
    Logic.System.step2
        (\( reproductionRate, _ ) ( populationSize, setPopulationSize ) ->
            setPopulationSize (Game.Components.scaledMultiply reproductionRate populationSize)
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
    Random.uniform Rocky
        [ Gas
        ]
        |> Random.andThen
            (\planetType ->
                Random.map3
                    (\orbit water size ->
                        ( planetId, world )
                            |> Logic.Entity.with ( Game.Components.celestialBodySpec, planetType )
                            |> Logic.Entity.with ( Game.Components.orbitSpec, orbit )
                            |> Logic.Entity.with ( Game.Components.waterSpec, water )
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
                    generatePlanetWaterPercent
                    (generatePlanetRadius planetType)
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
                map PlayingMessage (viewPlayering world)
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
                [ Ui.Text.defaultLabelLeft
                    []
                    { onChange = SetNameSingular
                    , id = "singular-name"
                    , text = model.civilizationNameSingular
                    , labelAttributes = [ width fill ]
                    , labelContent = text "Civilization Name Singular:"
                    }
                , Ui.Text.defaultLabelLeft
                    []
                    { onChange = SetNamePlural
                    , id = "plural-name"
                    , text = model.civilizationNamePlural
                    , labelAttributes = [ width fill ]
                    , labelContent = text "Civilization Name Plural:"
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


viewPlayering : World -> Element PlayingMsg
viewPlayering world =
    column
        [ width fill ]
        [ row
            [ padding 16, spacing 16 ]
            [ text "Game Speed:"
            , Ui.Button.toggle
                { label = text "||"
                , onPress = Just (SetTickRate Paused)
                , enabled = world.tickRate == Paused
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
            ]
        , row
            [ width fill ]
            [ column
                [ padding 16, alignTop ]
                [ Input.button
                    []
                    { label = text "Delete"
                    , onPress = Just DeleteGalaxy
                    }
                , case world.focus of
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
                            viewSlice (viewBody world viewPlanet planetId)

                        else
                            text "Missing planet"
                ]
            , viewPlayerCivilization world world.playerCiv
            ]
        ]


viewSlice : Element PlayingMsg -> Element PlayingMsg
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


viewBody : World -> (World -> EntityID -> Element PlayingMsg) -> EntityID -> Element PlayingMsg
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


viewGalaxy : World -> Element PlayingMsg
viewGalaxy model =
    Set.toList model.solarSystems
        |> List.map (viewSolarSystem model)
        |> column []


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
                |> List.map (Tuple.first >> viewPlanet model)
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


viewPlanet : World -> EntityID -> Element PlayingMsg
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


viewPlayerCivilization : World -> EntityID -> Element PlayingMsg
viewPlayerCivilization model civId =
    column
        [ padding 16, alignTop ]
        [ case Logic.Component.get2 civId model.named model.civilizationSizes of
            Nothing ->
                text "Your civ never reproduces"

            Just ( name, size ) ->
                text ("The " ++ Maybe.withDefault name.singular name.plural ++ " have " ++ sizeToString size ++ " citizens.")
        ]


sizeToString : ScaledNumber -> String
sizeToString size =
    case size of
        Millions f ->
            String.fromFloat f ++ " million"

        Billions f ->
            String.fromFloat f ++ " billion"
