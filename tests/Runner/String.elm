module Runner.String exposing (Summary, run, runWithOptions)

{-| # String Runner

Run a test and present its results as a nicely-formatted String, along with
a count of how many tests passed and failed.

Note that this always uses an initial seed of 902101337, since it can't do effects.

@docs Summary, run, runWithOptions
-}

import Expect exposing (Expectation)
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner exposing (Runner(..), SeededRunners)


{-| The output string, the number of passed tests,
and the number of failed tests.
-}
type alias Summary =
    { output : String, passed : Int, failed : Int, todos : List (List String) }


toOutput : Summary -> SeededRunners -> Summary
toOutput summary seededRunners =
    let
        result =
            List.foldl (toOutputHelp []) summary seededRunners.all
    in
        { result
            | output = result.output ++ outputTodos result.todos
        }


outputTodos : List (List String) -> String
outputTodos todos =
    todos
        |> List.map outputLabels
        |> String.join "\n\n"


toOutputHelp : List String -> Runner -> Summary -> Summary
toOutputHelp labels runner summary =
    case runner of
        Runnable runnable ->
            Test.Runner.run runnable
                |> List.foldl fromExpectation summary

        Labeled label subRunner ->
            toOutputHelp (label :: labels) subRunner summary

        Batch runners ->
            List.foldl (toOutputHelp labels) summary runners


fromExpectation : Expectation -> Summary -> Summary
fromExpectation expectation summary =
    case Test.Runner.getFailure expectation of
        Nothing ->
            { summary | passed = summary.passed + 1 }

        Just { given, message } ->
            let
                prefix =
                    case given of
                        Nothing ->
                            ""

                        Just g ->
                            "Given " ++ g ++ "\n\n"

                newOutput =
                    "\n\n" ++ (prefix ++ indentLines message) ++ "\n"
            in
                { summary
                    | output = summary.output ++ newOutput
                    , failed = summary.failed + 1
                    , passed = summary.passed
                }


outputLabels : List String -> String
outputLabels labels =
    labels
        |> Test.Runner.formatLabels ((++) "↓ ") ((++) "✗ ")
        |> String.join "\n"


defaultSeed : Random.Seed
defaultSeed =
    Random.initialSeed 902101337


defaultRuns : Int
defaultRuns =
    100


indentLines : String -> String
indentLines str =
    str
        |> String.split "\n"
        |> List.map ((++) "    ")
        |> String.join "\n"


{-| Run a test and return a tuple of the output message and the number of
tests that failed.

Fuzz tests use a default run count of 100, and a fixed initial seed.
-}
run : Test -> Summary
run =
    runWithOptions defaultRuns defaultSeed


{-| Run a test and return a tuple of the output message and the number of
tests that failed.
-}
runWithOptions : Int -> Random.Seed -> Test -> Summary
runWithOptions runs seed test =
    let
        seededRunners =
            Test.Runner.fromTest runs seed test
    in
        toOutput
            { output = ""
            , passed = 0
            , failed = 0
            , todos = seededRunners.todos
            }
            seededRunners
