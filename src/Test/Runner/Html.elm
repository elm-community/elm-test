module Test.Runner.Html exposing (run)

import Test exposing (Outcome, Test)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Test.Runner


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


dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity identity


init : List (() -> Outcome) -> ( Model, Cmd Msg )
init thunks =
    let
        indexedThunks : List ( TestId, () -> Outcome )
        indexedThunks =
            List.indexedMap (,) thunks

        model =
            { available = Dict.fromList indexedThunks
            , running = Set.empty
            , queue = List.map fst indexedThunks
            , completed = []
            }
    in
        ( model, dispatch )


run : Test -> Program Never
run test =
    Test.Runner.run
        { test = test
        , init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
