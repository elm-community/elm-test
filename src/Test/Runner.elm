module Test.Runner exposing (run)

import Test exposing (Test, Outcome, Suite)
import Html exposing (Html, text)
import Html.App
import Task
import Random.Pcg as Random
import Time exposing (Time)


type Msg subMsg
    = Init (Maybe Random.Seed)
    | SubMsg subMsg


type Model subMsg subModel
    = Uninitialized (SubUpdate subMsg subModel) Int Suite (List (() -> ( List String, Outcome )) -> ( subModel, Cmd subMsg ))
    | Initialized (SubUpdate subMsg subModel) subModel


getInitialSeed : Cmd (Msg a)
getInitialSeed =
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
        Uninitialized update runs suite init ->
            case msg of
                Init Nothing ->
                    ( Uninitialized update runs suite init, getInitialSeed )

                Init (Just seed) ->
                    let
                        ( subModel, subCmd ) =
                            init (toRunners seed runs suite)
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
        Uninitialized _ _ _ _ ->
            text ""

        Initialized _ subModel ->
            Html.App.map SubMsg (view subModel)


type alias SubUpdate msg model =
    msg -> model -> ( model, Cmd msg )


type alias RunnerOptions msg model =
    { suite : Suite
    , seed : Maybe Random.Seed
    , runs : Int
    , init : List (() -> ( List String, Outcome )) -> ( model, Cmd msg )
    , update : SubUpdate msg model
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


subscriptions : (subModel -> Sub subMsg) -> Model subMsg subModel -> Sub (Msg subMsg)
subscriptions subs model =
    case model of
        Uninitialized _ _ _ _ ->
            Sub.none

        Initialized _ subModel ->
            Sub.map SubMsg (subs subModel)


toRunners : Random.Seed -> Int -> Suite -> List (() -> ( List String, Outcome ))
toRunners =
    toRunnersHelp []


toRunnersHelp : List String -> Random.Seed -> Int -> Suite -> List (() -> ( List String, Outcome ))
toRunnersHelp labels seed runs suite =
    case suite of
        Test.Suite tests ->
            tests
                |> List.map (\test _ -> ( List.reverse labels, Test.run seed runs test ))

        Test.Labeled label suite ->
            toRunnersHelp (label :: labels) seed runs suite

        Test.Batch suites ->
            List.concatMap (toRunnersHelp labels seed runs) suites


run : RunnerOptions msg model -> Program Never
run opts =
    let
        init =
            case opts.seed of
                Just seed ->
                    let
                        ( subModel, subCmd ) =
                            opts.init (toRunners seed opts.runs opts.suite)
                    in
                        ( Initialized opts.update subModel, Cmd.map SubMsg subCmd )

                Nothing ->
                    ( Uninitialized opts.update opts.runs opts.suite opts.init, initCmd )
    in
        Html.App.program
            { init = init
            , update = initOrUpdate
            , view = initOrView opts.view
            , subscriptions = subscriptions opts.subscriptions
            }
