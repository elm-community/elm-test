module Test.Runner.Html exposing (run)

import Test exposing (Outcome, Test)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Random.Pcg as Random
import Time exposing (Time)


type alias TestId =
    Int


type alias Model =
    { available : Dict TestId (() -> Outcome)
    , running : Set TestId
    , queue : List TestId
    , completed : List Outcome
    }


type Msg
    = Dispatch


type InitMsg
    = Init (Maybe Random.Seed) Test
    | Msg Msg


viewOutcome : Outcome -> Html a
viewOutcome outcome =
    div [] [ text ("Outcome: " ++ toString outcome) ]


view : Model -> Html Msg
view model =
    let
        completionLabel =
            if Dict.isEmpty model.available && Set.isEmpty model.running then
                h2 [] [ text "Finished!" ]
            else
                text ""

        completedCount =
            List.length model.completed

        remainingCount =
            List.length (Dict.keys model.available)
    in
        div []
            [ h2 [] [ text "Running Tests..." ]
            , div [ class "results" ] (List.map viewOutcome model.completed)
            , completionLabel
            ]


warn : String -> a -> a
warn str result =
    let
        _ =
            Debug.log str
    in
        result


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dispatch ->
            case model.queue of
                [] ->
                    ( model, Cmd.none )
                        |> warn "Attempted to Dispatch when all tests completed!"

                testId :: _ ->
                    case Dict.get testId model.available of
                        Nothing ->
                            ( model, Cmd.none )
                                |> warn "Could not find testId"

                        Just run ->
                            let
                                completed =
                                    model.completed ++ [ run () ]

                                available =
                                    Dict.remove testId model.available

                                newModel =
                                    { model
                                        | completed = completed
                                        , available = available
                                    }

                                {- Dispatch as a Cmd so as to yield to the UI
                                   thread in between test executions.
                                -}
                            in
                                ( newModel, dispatch )


prepare : Random.Seed -> Test -> Model
prepare seed test =
    let
        available =
            testToDict seed test
    in
        { available = available
        , queue = List.sort (Dict.keys available)
        , completed = []
        , running = Set.empty
        }


testToDict : Random.Seed -> Test -> Dict TestId (() -> Outcome)
testToDict seed test =
    test
        |> Test.toRunners seed
        |> List.indexedMap (,)
        |> Dict.fromList


dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity identity


getInitialSeed : Test -> Cmd InitMsg
getInitialSeed test =
    Time.now
        |> Task.perform fromNever (\time -> Init (Just (seedFromTime time)) test)


seedFromTime : Time -> Random.Seed
seedFromTime time =
    (0xFFFFFFFF * time)
        |> floor
        |> Random.initialSeed


fromNever : Never -> a
fromNever a =
    fromNever a


initOrUpdate : InitMsg -> Maybe Model -> ( Maybe Model, Cmd InitMsg )
initOrUpdate msg maybeModel =
    case maybeModel of
        Nothing ->
            case msg of
                Init Nothing test ->
                    ( Nothing, getInitialSeed test )

                Init (Just seed) test ->
                    initOrUpdate (Msg Dispatch) (Just (prepare seed test))

                Msg _ ->
                    Debug.crash "Attempted to run a message pre-init!"

        Just model ->
            case msg of
                Msg subMsg ->
                    let
                        ( newModel, cmd ) =
                            update subMsg model
                    in
                        ( Just newModel, Cmd.map Msg cmd )

                Init _ _ ->
                    Debug.crash "Attempted to init twice!"


init : Test -> ( Maybe Model, Cmd InitMsg )
init test =
    let
        cmd =
            Task.succeed (Init Nothing test)
                |> Task.perform identity identity
    in
        ( Nothing, cmd )


initOrView : Maybe Model -> Html InitMsg
initOrView maybeModel =
    case maybeModel of
        Nothing ->
            text ""

        Just model ->
            Html.App.map Msg (view model)


run : Test -> Program Never
run test =
    Html.App.program
        { init = init test
        , update = initOrUpdate
        , view = initOrView
        , subscriptions = \_ -> Sub.none
        }
