module Helpers exposing (testStringLengthIsPreserved, expectToFail, testShrinking, randomSeedFuzzer)

import Test exposing (Test)
import Test.Expectation exposing (Expectation(..))
import Test.Internal as TI
import Fuzz exposing (Fuzzer)
import String
import Expect
import Random.Pcg as Random
import Shrink


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))


expectToFail : Test -> Test
expectToFail =
    expectFailureHelper (always Nothing)


expectFailureHelper : ({ description : String, given : Maybe String, reason : Test.Expectation.Reason } -> Maybe String) -> Test -> Test
expectFailureHelper f test =
    case test of
        TI.Test runTest ->
            TI.Test
                (\seed runs ->
                    let
                        expectations =
                            runTest seed runs

                        goodShrink expectation =
                            case expectation of
                                Pass ->
                                    Just "Expected this test to fail, but it passed!"

                                Fail record ->
                                    f record
                    in
                        expectations
                            |> List.filterMap goodShrink
                            |> List.map Expect.fail
                            |> (\list ->
                                    if List.isEmpty list then
                                        [ Expect.pass ]
                                    else
                                        list
                               )
                )

        TI.Labeled desc labeledTest ->
            TI.Labeled desc (expectFailureHelper f labeledTest)

        TI.Batch tests ->
            TI.Batch (List.map (expectFailureHelper f) tests)


testShrinking : Test -> Test
testShrinking =
    let
        handleFailure { given, description } =
            let
                acceptable =
                    String.split "|" description
            in
                case given of
                    Nothing ->
                        Just "Expected this test to have a given value!"

                    Just g ->
                        if List.member g acceptable then
                            Nothing
                        else
                            Just <| "Got shrunken value " ++ g ++ " but expected " ++ String.join " or " acceptable
    in
        expectFailureHelper handleFailure


{-| get a good distribution of random seeds, and don't shrink our seeds!
-}
randomSeedFuzzer : Fuzzer Random.Seed
randomSeedFuzzer =
    Fuzz.custom (Random.int 0 0xFFFFFFFF) Shrink.noShrink |> Fuzz.map Random.initialSeed
