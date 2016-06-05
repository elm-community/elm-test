module Test.Runner exposing (run)

import Test exposing (Outcome, Test)
import Html exposing (..)
import Html.App
import Task
import Random.Pcg as Random
import Time exposing (Time)


type Msg subMsg
    = Init (Maybe Random.Seed)
    | SubMsg subMsg


type Model subMsg subModel
    = Uninitialized (SubUpdate subMsg subModel) Test (List (() -> Outcome) -> ( subModel, Cmd subMsg ))
    | Initialized (SubUpdate subMsg subModel) subModel


getInitialSeed : Test -> Cmd (Msg a)
getInitialSeed test =
    Time.now
        |> Task.perform fromNever (\time -> Init (Just (timeToSeed time)))


timeToSeed : Time -> Random.Seed
timeToSeed time =
    (0xFFFFFFFF * time)
        |> floor
        |> Random.initialSeed


fromNever : Never -> a
fromNever a =
    fromNever a


initOrUpdate : Msg subMsg -> Model subMsg subModel -> ( Model subMsg subModel, Cmd (Msg subMsg) )
initOrUpdate msg maybeModel =
    case maybeModel of
        Uninitialized update test init ->
            case msg of
                Init Nothing ->
                    ( Uninitialized update test init, getInitialSeed test )

                Init (Just seed) ->
                    let
                        ( subModel, subCmd ) =
                            init (Test.toRunners seed test)
                    in
                        ( Initialized update subModel, Cmd.map SubMsg subCmd )

                SubMsg _ ->
                    Debug.crash "Attempted to run a SubMsg pre-Init!"

        Initialized update model ->
            case msg of
                SubMsg subMsg ->
                    let
                        ( newModel, cmd ) =
                            update subMsg model
                    in
                        ( Initialized update newModel, Cmd.map SubMsg cmd )

                Init _ ->
                    Debug.crash "Attempted to init twice!"


initCmd : Cmd (Msg a)
initCmd =
    Task.succeed (Init Nothing)
        |> Task.perform identity identity


initOrView : (subModel -> Html subMsg) -> Model subMsg subModel -> Html (Msg subMsg)
initOrView view model =
    case model of
        Uninitialized _ _ _ ->
            text ""

        Initialized _ subModel ->
            Html.App.map SubMsg (view subModel)


type alias SubUpdate msg model =
    msg -> model -> ( model, Cmd msg )


type alias RunnerOptions msg model =
    { test : Test
    , init : List (() -> Outcome) -> ( model, Cmd msg )
    , update : SubUpdate msg model
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


subscriptions : (subModel -> Sub subMsg) -> Model subMsg subModel -> Sub (Msg subMsg)
subscriptions subs model =
    case model of
        Uninitialized _ _ _ ->
            Sub.none

        Initialized _ subModel ->
            Sub.map SubMsg (subs subModel)


run : RunnerOptions msg model -> Program Never
run opts =
    Html.App.program
        { init = ( Uninitialized opts.update opts.test opts.init, initCmd )
        , update = initOrUpdate
        , view = initOrView opts.view
        , subscriptions = subscriptions opts.subscriptions
        }
