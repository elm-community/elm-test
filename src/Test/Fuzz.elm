module Test.Fuzz exposing (fuzzTest)

import Dict exposing (Dict)
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), failNow)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal exposing (unpackGenVal, unpackGenTree)
import RoseTree exposing (RoseTree(..))
import Lazy.List
import Random.Pcg as Random exposing (Generator)


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest ((Fuzz.Internal.Fuzzer baseFuzzer) as fuzzer) untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
        if desc == "" then
            failNow
                { description = "You must pass your fuzz tests a nonempty string!"
                , reason = Test.Expectation.Invalid Test.Expectation.BadDescription
                }
        else
            case Fuzz.Internal.invalidReason (baseFuzzer True) of
                Just reason ->
                    failNow
                        { description = reason
                        , reason = Test.Expectation.Invalid Test.Expectation.InvalidFuzzer
                        }

                Nothing ->
                    -- Preliminary checks passed; run the fuzz test
                    validatedFuzzTest fuzzer desc getExpectation


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
validatedFuzzTest fuzzer desc getExpectation =
    let
        run seed runs =
            let
                failures =
                    getFailures fuzzer getExpectation seed runs
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


type alias Failures =
    Dict String Expectation


getFailures : Fuzzer a -> (a -> Expectation) -> Random.Seed -> Int -> Dict String Expectation
getFailures fuzzer getExpectation initialSeed totalRuns =
    {- Fuzz test algorithm with memoization and opt-in RoseTrees:
       Generate a single value from the fuzzer's genVal random generator
       Determine if the value is memoized. If so, skip. Otherwise continue.
       Run the test on that value. If it fails:
           Generate the rosetree by passing the fuzzer False *and the same random seed*
           Find the new failure by looking at the children for any shrunken values:
               If a shrunken value causes a failure, recurse on its children
               If no shrunken value replicates the failure, use the root
       Whether it passes or fails, do this n times
    -}
    let
        genVal =
            unpackGenVal fuzzer

        initialFailures =
            Dict.empty

        helper currentSeed remainingRuns failures =
            let
                ( value, nextSeed ) =
                    Random.step genVal currentSeed
            in
                let
                    newFailures =
                        findNewFailure fuzzer getExpectation failures currentSeed value
                in
                    if remainingRuns == 1 then
                        newFailures
                    else
                        helper nextSeed (remainingRuns - 1) newFailures
    in
        helper initialSeed totalRuns initialFailures


{-| Knowing that a value in not in the cache, determine if it causes the test to pass or fail.
-}
findNewFailure :
    Fuzzer a
    -> (a -> Expectation)
    -> Failures
    -> Random.Seed
    -> a
    -> Failures
findNewFailure fuzzer getExpectation failures currentSeed value =
    case getExpectation value of
        Pass ->
            failures

        failedExpectation ->
            let
                genTree =
                    unpackGenTree fuzzer

                ( rosetree, nextSeed ) =
                    -- nextSeed is not used here because caller function has currentSeed
                    Random.step genTree currentSeed
            in
                shrinkAndAdd rosetree getExpectation failedExpectation failures


{-| Knowing that the rosetree's root already failed, finds the shrunken failure.
Returns the updated failures dictionary.
-}
shrinkAndAdd :
    RoseTree a
    -> (a -> Expectation)
    -> Expectation
    -> Failures
    -> Failures
shrinkAndAdd rootTree getExpectation rootsExpectation failures =
    let
        -- needs annotation
        shrink oldExpectation (Rose failingValue branches) =
            case Lazy.List.headAndTail branches of
                Just ( (Rose possiblyFailingValue _) as rosetree, moreLazyRoseTrees ) ->
                    -- either way, recurse with the most recent failing expectation, and failing input with its list of shrunken values
                    case getExpectation possiblyFailingValue of
                        Pass ->
                            shrink oldExpectation
                                (Rose failingValue moreLazyRoseTrees)

                        newExpectation ->
                            let
                                ( minimalValue, finalExpectation ) =
                                    shrink newExpectation rosetree
                            in
                                ( minimalValue
                                , finalExpectation
                                )

                Nothing ->
                    ( failingValue, oldExpectation )

        (Rose failingValue _) =
            rootTree

        ( minimalValue, finalExpectation ) =
            shrink rootsExpectation rootTree
    in
        Dict.insert (toString minimalValue) finalExpectation failures


formatExpectation : ( String, Expectation ) -> Expectation
formatExpectation ( given, expectation ) =
    Test.Expectation.withGiven given expectation
