module Test.Test exposing (Test(..), fuzzTest)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Dict exposing (Dict)
import Shrink exposing (Shrinker)
import Fuzz exposing (Fuzzer)
import Expect


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


fuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest { generator, shrinker } desc getExpectation =
    let
        run seed runs =
            if runs < 1 then
                [ Expect.fail ("Fuzz test run count must be at least 1, not " ++ toString runs) ]
            else
                let
                    getFailures failures currentSeed remainingRuns =
                        let
                            ( val, nextSeed ) =
                                Random.step generator currentSeed

                            newFailures =
                                if getExpectation val == Pass then
                                    failures
                                else
                                    shrinkAndAdd shrinker getExpectation val failures
                        in
                            if remainingRuns == 1 then
                                newFailures
                            else
                                getFailures newFailures nextSeed (remainingRuns - 1)

                    -- Use a Dict so we don't report duplicate inputs.
                    failures : Dict String Expectation
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
formatExpectation ( given, expectation ) =
    Test.Expectation.withGiven given expectation


isFail : Expectation -> Bool
isFail =
    (/=) Pass
