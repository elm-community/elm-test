module Test.Runner exposing (run)

import Test exposing (Outcome, Test)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Random.Pcg as Random
import Time exposing (Time)


type Msg subMsg
    = Init (Maybe Random.Seed)
    | SubMsg subMsg

type InitModel subMsg subModel  =
    | Uninitialized Test ( subModel, Cmd subMsg )
    | Initialized subModel



getInitialSeed : Test -> Cmd InitMsg
getInitialSeed test =
    Time.now
        |> Task.perform fromNever (\time -> Init (Just (timeToSeed time)) test)


timeToSeed : Time -> Random.Seed
timeToSeed time =
    (0xFFFFFFFF * time)
        |> floor
        |> Random.initialSeed


fromNever : Never -> a
fromNever a =
    fromNever a


initOrUpdate : Msg subMsg -> InitModel subMsg subModel -> ( InitModel subMsg subModel, Cmd Msg )
initOrUpdate msg maybeModel =
    case maybeModel of
        Uninitialized test ( subModel, subCmd ) ->
            case msg of
                Init Nothing ->
                    ( Uninitialized test ( subModel, subCmd )
                        , getInitialSeed test )

                Init (Just seed) ->
                    ( Initialized subModel, Cmd.map SubCmd subCmd)

                SubMsg _ ->
                    Debug.crash "Attempted to run a SubMsg pre-Init!"

        Initialized model ->
            case msg of
                SubMsg subMsg ->
                    let
                        ( newModel, cmd ) =
                            update subMsg model
                    in
                        ( Just newModel, Cmd.map SubMsg cmd )

                Init _ _ ->
                    Debug.crash "Attempted to init twice!"


init : Test -> ( Maybe model, Cmd (Msg subMsg) )
init test =
    let
        cmd =
            Task.succeed (Init Nothing test)
                |> Task.perform identity identity
    in
        ( Nothing, cmd )


initOrView : Maybe (InitModel subModel) -> Html (Msg subMsg)
initOrView maybeModel =
    case maybeModel of
        Nothing ->
            text ""

        Just model ->
            Html.App.map SubMsg (view model)


type alias RunnerOptions msg model =
    { test : Test
    , update : model -> msg -> ( model, Cmd msg )
    , view : model -> Html msg
    }


run : RunnerOptions msg model -> Program Never
run opts =
    Html.App.program
        { init = init opts.test
        , update = initOrUpdate opts.update
        , view = initOrView opts.view
        , subscriptions = \_ -> Sub.none
        }
