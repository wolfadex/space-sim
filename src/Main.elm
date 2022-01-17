module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Element exposing (..)
import NewGame
import Playing
import Random
import Shared exposing (Effect(..), Flags)
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


type Model
    = NewGame NewGame.Model
    | Playing Playing.World


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( newGameModel, initializeNewGame ) =
            SubModule.initWithEffect
                { toMsg = GotNewGameMessage
                , effectToMsg = GotSharedEffect
                }
                (NewGame.init (Random.initialSeed flags.seed0))
    in
    ( NewGame newGameModel
    , Cmd.none
    )
        |> initializeNewGame


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NewGame _ ->
            Sub.none

        Playing world ->
            Sub.map GotPlayingMessage (Playing.subscriptions world)


type Msg
    = GotNewGameMessage NewGame.Msg
    | GotPlayingMessage Playing.Msg
    | GotSharedEffect Effect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotNewGameMessage message, NewGame newGameModel ) ->
            NewGame.update message newGameModel
                |> SubModule.updateWithEffect
                    { toMsg = GotNewGameMessage
                    , effectToMsg = GotSharedEffect
                    , toModel = NewGame
                    }

        ( GotPlayingMessage message, Playing world ) ->
            Playing.update message world
                |> SubModule.updateWithEffect
                    { toMsg = GotPlayingMessage
                    , effectToMsg = GotSharedEffect
                    , toModel = Playing
                    }

        ( GotSharedEffect effect, _ ) ->
            case effect of
                CreateGame newGameDetails ->
                    let
                        ( playingModel, initializePlaying ) =
                            SubModule.initWithEffect
                                { toMsg = GotPlayingMessage
                                , effectToMsg = GotSharedEffect
                                }
                                (Playing.init newGameDetails)
                    in
                    ( Playing playingModel
                    , Cmd.none
                    )
                        |> initializePlaying

                DeleteGame seed ->
                    let
                        ( newGameModel, initializeNewGame ) =
                            SubModule.initWithEffect
                                { toMsg = GotNewGameMessage
                                , effectToMsg = GotSharedEffect
                                }
                                (NewGame.init seed)
                    in
                    ( NewGame newGameModel
                    , Cmd.none
                    )
                        |> initializeNewGame

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        content : View Msg
        content =
            case model of
                NewGame m ->
                    View.map GotNewGameMessage (NewGame.view m)

                Playing m ->
                    View.map GotPlayingMessage (Playing.view m)
    in
    { title = content.title
    , body = [ layout [ width fill, height fill ] content.body ]
    }
