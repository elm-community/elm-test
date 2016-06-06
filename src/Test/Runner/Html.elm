module Test.Runner.Html exposing (run)

import Test exposing (Test)
import Assert exposing (Outcome)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Test.Runner
import String


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


viewFailures : List String -> List (Html a)
viewFailures messages =
    case messages of
        [] ->
            []

        final :: [] ->
            [ text final ]

        penultimate :: final :: [] ->
            [ withRedChar '✗' penultimate, p [] [ text final ] ]

        first :: rest ->
            withRedChar '↓' first :: viewFailures rest


withRedChar : Char -> String -> Html a
withRedChar char str =
    div []
        [ span [ style [ ( "color", "hsla(3, 100%, 40%, 1.0)" ) ] ] [ text (String.fromChar char) ]
        , span [] [ text (String.cons ' ' str) ]
        ]


view : Model -> Html Msg
view model =
    let
        isFinished =
            Dict.isEmpty model.available && Set.isEmpty model.running

        summary =
            if isFinished then
                if List.isEmpty failures then
                    h2 [ style [ ( "color", "darkgreen" ) ] ] [ text "All tests passed!" ]
                else
                    h2 [ style [ ( "color", "hsla(3, 100%, 40%, 1.0)" ) ] ] [ text (toString (List.length failures) ++ " of " ++ toString completedCount ++ " Tests Failed:") ]
            else
                div []
                    [ h2 [] [ text "Running Tests..." ]
                    , div [] [ text (toString completedCount ++ " completed") ]
                    , div [] [ text (toString remainingCount ++ " remaining") ]
                    ]

        completedCount =
            List.length model.completed

        remainingCount =
            List.length (Dict.keys model.available)

        failures : List Outcome
        failures =
            Assert.withoutSuccesses model.completed
    in
        div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
            [ summary
            , ol [ class "results", style [ ( "font-family", "monospace" ) ] ] (List.map viewOutcome failures)
            ]


viewOutcome : Outcome -> Html a
viewOutcome outcome =
    outcome
        |> Assert.toFailures
        |> viewFailures
        |> li [ style [ ( "margin", "40px 0" ) ] ]


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

                testId :: newQueue ->
                    case Dict.get testId model.available of
                        Nothing ->
                            ( model, Cmd.none )
                                |> warn ("Could not find testId " ++ toString testId)

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
                                        , queue = newQueue
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
