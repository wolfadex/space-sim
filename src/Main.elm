port module Main exposing (Model, Msg, Page, main)

import Browser exposing (Document)
import Element exposing (..)
import Game.Components
import Json.Encode exposing (Value)
import NewGame
import Playing
import Shared exposing (Effect(..), Flags, SharedModel)
import SubModule
import View exposing (View)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { shared : SharedModel
    , page : Page
    }


type Page
    = NewGame NewGame.Model
    | Playing Game.Components.World


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( newGameModel, initializeNewGame ) =
            SubModule.initWithEffect
                { toMsg = GotNewGameMessage
                , effectToMsg = GotSharedEffect
                }
                NewGame.init
    in
    initializeNewGame
        ( { shared = Shared.init flags
          , page = NewGame newGameModel
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NewGame m ->
            Sub.map GotNewGameMessage (NewGame.subscriptions m)

        Playing world ->
            Sub.map GotPlayingMessage (Playing.subscriptions world)


type Msg
    = GotNewGameMessage NewGame.Msg
    | GotPlayingMessage Game.Components.PlayingMsg
    | GotSharedEffect Effect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotNewGameMessage message, NewGame newGameModel ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = GotNewGameMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = NewGame
                        }
                        (NewGame.update model.shared message newGameModel)
            in
            ( { model | page = pageModel }, cmd )

        ( GotPlayingMessage message, Playing world ) ->
            let
                ( pageModel, cmd ) =
                    SubModule.updateWithEffect
                        { toMsg = GotPlayingMessage
                        , effectToMsg = GotSharedEffect
                        , toModel = Playing
                        }
                        (Playing.update model.shared message world)
            in
            ( { model | page = pageModel }, cmd )

        ( GotSharedEffect effect, _ ) ->
            case effect of
                CreateGame playType newGameDetails ->
                    let
                        ( playingModel, initializePlaying ) =
                            SubModule.initWithEffect
                                { toMsg = GotPlayingMessage
                                , effectToMsg = GotSharedEffect
                                }
                                (Playing.init model.shared playType newGameDetails)
                    in
                    initializePlaying
                        ( { model | page = Playing playingModel }
                        , Cmd.none
                        )

                DeleteGame ->
                    let
                        ( newGameModel, initializeNewGame ) =
                            SubModule.initWithEffect
                                { toMsg = GotNewGameMessage
                                , effectToMsg = GotSharedEffect
                                }
                                NewGame.init
                    in
                    initializeNewGame
                        ( { model | page = NewGame newGameModel }
                        , Cmd.none
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

        _ ->
            ( model, Cmd.none )


port saveSettings : Value -> Cmd msg


view : Model -> Document Msg
view model =
    let
        content : View Msg
        content =
            case model.page of
                NewGame m ->
                    View.map GotNewGameMessage (NewGame.view model.shared m)

                Playing m ->
                    View.map GotPlayingMessage (Playing.view model.shared m)
    in
    { title = content.title
    , body = [ layout [ width fill, height fill ] content.body ]
    }
