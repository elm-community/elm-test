module Test.Runner exposing (run, toRunners)

import Test exposing (Test, Suite)
import Test.Outcome exposing (Outcome)
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


type alias RunnerOptions =
    { seed : Maybe Random.Seed
    , runs : Maybe Int
    }


type alias AppOptions msg model =
    { init : List (() -> ( List String, Outcome )) -> ( model, Cmd msg )
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
                |> List.map (\test _ -> ( List.reverse labels, test { seed = seed, runs = runs } ))

        Test.Labeled label suite ->
            toRunnersHelp (label :: labels) seed runs suite

        Test.Batch suites ->
            List.concatMap (toRunnersHelp labels seed runs) suites


run : RunnerOptions -> AppOptions msg model -> Suite -> Program Never
run runnerOpts appOpts suite =
    let
        runs =
            Maybe.withDefault defaultRunCount runnerOpts.runs

        init =
            case runnerOpts.seed of
                Just seed ->
                    let
                        ( subModel, subCmd ) =
                            appOpts.init (toRunners seed runs suite)
                    in
                        ( Initialized appOpts.update subModel, Cmd.map SubMsg subCmd )

                Nothing ->
                    ( Uninitialized appOpts.update runs suite appOpts.init, initCmd )
    in
        Html.App.program
            { init = init
            , update = initOrUpdate
            , view = initOrView appOpts.view
            , subscriptions = subscriptions appOpts.subscriptions
            }


defaultRunCount : Int
defaultRunCount =
    100
