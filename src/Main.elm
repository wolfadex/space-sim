port module Main exposing (Model, Msg, Page, main)

import Browser exposing (Document)
import Browser.Navigation
import Element exposing (..)
import Game.Components
import Json.Encode exposing (Value)
import Page.Home
import Page.NewGameObserve
import Page.NewGameParticipate
import Page.Playing
import Route
import Shared exposing (Effect(..), Flags, SharedModel)
import SubModule
import Url
import View exposing (View)
import WebAudio


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


type alias Model =
    { shared : SharedModel
    , page : Page
    , navKey : Browser.Navigation.Key
    }


type Page
    = PageHome Page.Home.Model
    | PageNewGameParticipate Page.NewGameParticipate.Model
    | PageNewGameObserve Page.NewGameObserve.Model
    | PagePlaying Game.Components.World


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( initialPage, initializeApp ) =
            initPageFromUrl url
    in
    initializeApp
        ( { shared = Shared.init flags
          , page = initialPage
          , navKey = navKey
          }
        , Cmd.none
        )


initPageFromUrl : Url.Url -> ( Page, ( Model, Cmd Msg ) -> ( Model, Cmd Msg ) )
initPageFromUrl url =
    case Route.fromUrl url of
        Route.Home ->
            SubModule.initWithEffect
                { toMsg = PageHomeMessage
                , effectToMsg = GotSharedEffect
                }
                Page.Home.init
                |> Tuple.mapFirst PageHome

        Route.NewGameParticipate ->
            SubModule.initWithEffect
                { toMsg = PageNewGameParticipateMessage
                , effectToMsg = GotSharedEffect
                }
                Page.NewGameParticipate.init
                |> Tuple.mapFirst PageNewGameParticipate

        Route.NewGameObserve ->
            SubModule.initWithEffect
                { toMsg = PageNewGameObserveMessage
                , effectToMsg = GotSharedEffect
                }
                Page.NewGameObserve.init
                |> Tuple.mapFirst PageNewGameObserve

        Route.Playing generationConfig ->
            SubModule.initWithEffect
                { toMsg = PagePlayingMessage
                , effectToMsg = GotSharedEffect
                }
                (Page.Playing.init generationConfig)
                |> Tuple.mapFirst PagePlaying


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PageHome m ->
            Sub.map PageHomeMessage (Page.Home.subscriptions m)

        PageNewGameParticipate m ->
            Sub.map PageNewGameParticipateMessage (Page.NewGameParticipate.subscriptions m)

        PageNewGameObserve m ->
            Sub.map PageNewGameObserveMessage (Page.NewGameObserve.subscriptions m)

        PagePlaying world ->
            Sub.map PagePlayingMessage (Page.Playing.subscriptions world)


type Msg
    = GotSharedEffect Effect
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
    | PagePlayingMessage Game.Components.PlayingMsg
    | PageHomeMessage Page.Home.Msg
    | PageNewGameParticipateMessage Page.NewGameParticipate.Msg
    | PageNewGameObserveMessage Page.NewGameObserve.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( OnUrlChange url, _ ) ->
            let
                ( initialPage, initializeApp ) =
                    initPageFromUrl url
            in
            initializeApp
                ( { model | page = initialPage }
                , Cmd.none
                )

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        ( PageHomeMessage message, PageHome m ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = PageHomeMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = PageHome
                        }
                        (Page.Home.update model.shared message m)
            in
            ( { model | page = pageModel }, cmd )

        ( PageNewGameParticipateMessage message, PageNewGameParticipate m ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = PageNewGameParticipateMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = PageNewGameParticipate
                        }
                        (Page.NewGameParticipate.update model.shared message m)
            in
            ( { model | page = pageModel }, cmd )

        ( PageNewGameObserveMessage message, PageNewGameObserve m ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = PageNewGameObserveMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = PageNewGameObserve
                        }
                        (Page.NewGameObserve.update model.shared message m)
            in
            ( { model | page = pageModel }, cmd )

        ( PagePlayingMessage message, PagePlaying world ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = PagePlayingMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = PagePlaying
                        }
                        (Page.Playing.update model.shared message world)
            in
            ( { model | page = pageModel }, cmd )

        ( GotSharedEffect effect, _ ) ->
            case effect of
                DeleteGame ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey (Route.toString Route.Home)
                    )

                GotSharedMessage change ->
                    let
                        shared : SharedModel
                        shared =
                            Shared.update change model.shared
                    in
                    ( { model | shared = shared }, saveSettings (Shared.encodeSettings shared.settings) )

                UpdateSeed seed ->
                    let
                        shared : SharedModel
                        shared =
                            model.shared
                    in
                    ( { model | shared = { shared | seed = seed } }, Cmd.none )

                PlayAudio audio ->
                    ( model
                    , toWebAudio (Json.Encode.list WebAudio.encode audio)
                    )

        _ ->
            ( model, Cmd.none )


port saveSettings : Value -> Cmd msg


port toWebAudio : Value -> Cmd msg


view : Model -> Document Msg
view model =
    let
        content : View Msg
        content =
            case model.page of
                PageHome m ->
                    View.map PageHomeMessage (Page.Home.view model.shared m)

                PageNewGameParticipate m ->
                    View.map PageNewGameParticipateMessage (Page.NewGameParticipate.view model.shared m)

                PageNewGameObserve m ->
                    View.map PageNewGameObserveMessage (Page.NewGameObserve.view model.shared m)

                PagePlaying m ->
                    View.map PagePlayingMessage (Page.Playing.view model.shared m)
    in
    { title = content.title
    , body = [ layout [ width fill, height fill ] content.body ]
    }
