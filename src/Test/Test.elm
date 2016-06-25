module Test.Test exposing (Test(..), fuzzTest)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Dict exposing (Dict)
import Shrink exposing (Shrinker)
import Fuzz exposing (Fuzzer)


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


fuzzTest : String -> Fuzzer a -> (a -> Expectation) -> Test
fuzzTest desc { generator, shrinker } getExpectation =
    let
        run seed runs =
            if runs < 1 then
                [ Fail ("Fuzz test run count must be at least 1, not " ++ toString runs) ]
            else
                let
                    getFailures failures currentSeed remainingRuns =
                        let
                            ( val, nextSeed ) =
                                Random.step generator currentSeed

                            newFailures =
                                case getExpectation val of
                                    Pass ->
                                        failures

                                    failure ->
                                        shrinkAndAdd shrinker getExpectation val failures
                        in
                            if remainingRuns == 1 then
                                newFailures
                            else
                                getFailures newFailures nextSeed (remainingRuns - 1)

                    -- Use a Dict so we don't report duplicate inputs.
                    failures =
                        getFailures Dict.empty seed runs
                in
                    -- Make sure if we passed, we don't do any more work.
                    if Dict.isEmpty failures then
                        [ Pass ]
                    else
                        failures
                            |> Dict.toList
                            |> List.map formatExpectation
    in
        Labeled desc (Test run)


shrinkAndAdd : Shrinker a -> (a -> Expectation) -> a -> Dict String Expectation -> Dict String Expectation
shrinkAndAdd shrinker getExpectation val dict =
    let
        result =
            Shrink.shrink (getExpectation >> isFail) shrinker val
    in
        Dict.insert (toString result) (getExpectation result) dict


formatExpectation : ( String, Expectation ) -> Expectation
formatExpectation ( input, expectation ) =
    Test.Expectation.formatFailure (prependInput input) expectation


prependInput : String -> String -> String
prependInput input original =
    "► Given " ++ input ++ "\n\n" ++ original


isFail : Expectation -> Bool
isFail =
    (/=) Pass
