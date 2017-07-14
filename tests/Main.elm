module Main exposing (..)

{-| HOW TO RUN THESE TESTS

$ npm test

Note that this always uses an initial seed of 902101337, since it can't do effects.

-}

import Platform
import Random.Pcg as Random
import Runner.Log
import Runner.String exposing (Summary)
import SeedTests
import Test exposing (Test)
import Tests


main : Program Never () msg
main =
    Platform.program
        { init = ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
        |> Runner.Log.run Tests.all
        -- All seed tests should pass because they receive the same initial seed.
        |> runAllTests 1 SeedTests.fixedSeed SeedTests.tests


runAllTests : Int -> Random.Seed -> List Test -> a -> a
runAllTests runs seed tests =
    tests
        |> List.foldl
            (\test summary ->
                Runner.String.runWithOptions runs seed test
                    |> combineSummaries summary
            )
            emptySummary
        |> Runner.Log.logOutput


emptySummary : Summary
emptySummary =
    { output = "", passed = 0, failed = 0, autoFail = Nothing }


combineSummaries : Summary -> Summary -> Summary
combineSummaries first second =
    { output = first.output ++ second.output
    , passed = first.passed + second.passed
    , failed = first.failed + second.failed
    , autoFail =
        case ( first.autoFail, second.autoFail ) of
            ( Nothing, Nothing ) ->
                Nothing

            ( Nothing, second ) ->
                second

            ( first, Nothing ) ->
                first

            ( Just first, Just second ) ->
                [ first, second ]
                    |> String.join "\n"
                    |> Just
    }
