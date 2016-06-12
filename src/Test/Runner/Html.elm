module Test.Runner.Html exposing (run)

import Test exposing (Test, Suite)
import Test.Outcome exposing (Outcome)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Test.Runner
import String
import Random.Pcg as Random


type alias TestId =
    Int


type alias Model =
    { available : Dict TestId (() -> ( List String, Outcome ))
    , running : Set TestId
    , queue : List TestId
    , completed : List ( List String, Outcome )
    }


type Msg
    = Dispatch


viewFailures : String -> List String -> List (Html a)
viewFailures message context =
    let
        ( maybeLastContext, otherContexts ) =
            case List.reverse context of
                [] ->
                    ( Nothing, [] )

                first :: rest ->
                    ( Just first, List.reverse rest )

        viewMessage message =
            case maybeLastContext of
                Just lastContext ->
                    div []
                        [ withColorChar '✗' "hsla(3, 100%, 40%, 1.0)" lastContext
                        , pre [] [ text message ]
                        ]

                Nothing ->
                    pre [] [ text message ]

        viewContext =
            otherContexts
                |> List.map (withColorChar '↓' "darkgray")
                |> div []
    in
        [ viewContext ] ++ [ viewMessage message ]


withColorChar : Char -> String -> String -> Html a
withColorChar char textColor str =
    div [ style [ ( "color", textColor ) ] ]
        [ text (String.cons char (String.cons ' ' str)) ]


view : Model -> Html Msg
view model =
    let
        isFinished =
            Dict.isEmpty model.available && Set.isEmpty model.running

        summary =
            if isFinished then
                if List.isEmpty failures then
                    h2 [ style [ ( "color", "darkgreen" ) ] ] [ text "All Suites passed!" ]
                else
                    h2 [ style [ ( "color", "hsla(3, 100%, 40%, 1.0)" ) ] ] [ text (toString (List.length failures) ++ " of " ++ toString completedCount ++ " Suites Failed:") ]
            else
                div []
                    [ h2 [] [ text "Running Suites..." ]
                    , div [] [ text (toString completedCount ++ " completed") ]
                    , div [] [ text (toString remainingCount ++ " remaining") ]
                    ]

        completedCount =
            List.length model.completed

        remainingCount =
            List.length (Dict.keys model.available)

        failures : List ( List String, Outcome )
        failures =
            List.filter (snd >> (/=) Test.Outcome.pass) model.completed
    in
        div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
            [ summary
            , ol [ class "results", style [ ( "font-family", "monospace" ) ] ] (viewContextualOutcomes failures)
            ]


viewContextualOutcomes : List ( List String, Outcome ) -> List (Html a)
viewContextualOutcomes =
    List.concatMap viewOutcome


viewOutcome : ( List String, Outcome ) -> List (Html a)
viewOutcome ( descriptions, outcome ) =
    case Test.Outcome.toFailures outcome of
        Just failures ->
            failures
                |> List.map (\failure -> li [ style [ ( "margin", "40px 0" ) ] ] (viewFailures failure descriptions))

        Nothing ->
            []


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
                        |> warn "Attempted to Dispatch when all Suites completed!"

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
                                   thread in between Suite executions.
                                -}
                            in
                                ( newModel, dispatch )


dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity identity


init : List (() -> ( List String, Outcome )) -> ( Model, Cmd Msg )
init thunks =
    let
        indexedThunks : List ( TestId, () -> ( List String, Outcome ) )
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


run : Suite -> Program Never
run =
    runWithOptions Nothing 100


runWithOptions : Maybe Random.Seed -> Int -> Suite -> Program Never
runWithOptions maybeSeed runs suite =
    Test.Runner.run
        { suite = suite
        , runs = runs
        , seed = maybeSeed
        , init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
