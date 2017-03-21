module Test.Fuzz exposing (fuzzTest)

import Dict exposing (Dict)
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), failNow, blankDescriptionFailure)
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
        if String.isEmpty desc then
            blankDescriptionFailure
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


type ShrinkingResult a
    = Passes
    | ShrinksTo a


type alias State a =
    { failures : Dict String Expectation
    , results : Dict String (ShrinkingResult a)
    }


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

        initialState =
            State Dict.empty Dict.empty

        helper currentSeed remainingRuns state =
            let
                ( value, nextSeed ) =
                    Random.step genVal currentSeed
            in
                case Dict.get (toString value) state.results of
                    Just _ ->
                        -- we can skip this, already have the result in `failures`
                        state.failures

                    Nothing ->
                        let
                            newState =
                                findNewFailure fuzzer getExpectation state currentSeed value
                        in
                            if remainingRuns == 1 then
                                newState.failures
                            else
                                helper nextSeed (remainingRuns - 1) newState
    in
        helper initialSeed totalRuns initialState


{-| Knowing that a value in not in the cache, determine if it causes the test to pass or fail.
-}
findNewFailure :
    Fuzzer a
    -> (a -> Expectation)
    -> State a
    -> Random.Seed
    -> a
    -> State a
findNewFailure fuzzer getExpectation state currentSeed value =
    case getExpectation value of
        Pass ->
            { state | results = Dict.insert (toString value) Passes state.results }

        failedExpectation ->
            let
                genTree =
                    unpackGenTree fuzzer

                ( rosetree, nextSeed ) =
                    -- nextSeed is not used here because caller function has currentSeed
                    Random.step genTree currentSeed
            in
                shrinkAndAdd rosetree getExpectation failedExpectation state


{-| Knowing that the rosetree's root already failed, but that it's not in the results cache, finds the shrunken failure.
Returns the updated state (failures dictionary and results cache dictionary).
-}
shrinkAndAdd :
    RoseTree a
    -> (a -> Expectation)
    -> Expectation
    -> State a
    -> State a
shrinkAndAdd rootTree getExpectation rootsExpectation { failures, results } =
    let
        shrink oldExpectation (Rose failingValue branches) results =
            case Lazy.List.headAndTail branches of
                Just ( (Rose possiblyFailingValue _) as rosetree, moreLazyRoseTrees ) ->
                    -- either way, recurse with the most recent failing expectation, and failing input with its list of shrunken values
                    case Dict.get (toString possiblyFailingValue) results of
                        Just result ->
                            case result of
                                Passes ->
                                    shrink oldExpectation (Rose failingValue moreLazyRoseTrees) results

                                ShrinksTo minimalValue ->
                                    ( minimalValue, oldExpectation, results )

                        Nothing ->
                            case getExpectation possiblyFailingValue of
                                Pass ->
                                    shrink oldExpectation
                                        (Rose failingValue moreLazyRoseTrees)
                                        (Dict.insert (toString possiblyFailingValue) Passes results)

                                newExpectation ->
                                    let
                                        ( minimalValue, finalExpectation, newResults ) =
                                            shrink newExpectation rosetree results
                                    in
                                        ( minimalValue
                                        , finalExpectation
                                        , newResults
                                            |> Dict.insert (toString possiblyFailingValue) (ShrinksTo minimalValue)
                                            |> Dict.insert (toString failingValue) (ShrinksTo minimalValue)
                                        )

                Nothing ->
                    ( failingValue
                    , oldExpectation
                    , Dict.insert (toString failingValue) (ShrinksTo failingValue) results
                      -- minimal value!
                    )

        (Rose failingValue _) =
            rootTree

        ( minimalValue, finalExpectation, newResults ) =
            shrink rootsExpectation rootTree results
    in
        { failures = Dict.insert (toString minimalValue) finalExpectation failures
        , results = Dict.insert (toString failingValue) (ShrinksTo minimalValue) newResults
        }


formatExpectation : ( String, Expectation ) -> Expectation
formatExpectation ( given, expectation ) =
    Test.Expectation.withGiven given expectation
