module Test.Internal exposing (Test(..), failNow, fuzzTest, filter)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal exposing (unpackGenVal, unpackGenTree)
import RoseTree exposing (RoseTree(..))
import Lazy.List


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Test.Expectation.Reason } -> Test
failNow record =
    Test
        (\_ _ -> [ Test.Expectation.fail record ])


filter : (String -> Bool) -> Test -> Test
filter =
    filterHelp False


filterHelp : Bool -> (String -> Bool) -> Test -> Test
filterHelp lastCheckPassed isKeepable test =
    case test of
        Test _ ->
            if lastCheckPassed then
                test
            else
                Batch []

        Labeled desc labeledTest ->
            labeledTest
                |> filterHelp (isKeepable desc) isKeepable
                |> Labeled desc

        Batch tests ->
            tests
                |> List.map (filterHelp lastCheckPassed isKeepable)
                |> Batch


type ShrinkingResult a
    = Passes
    | ShrinksTo a


type alias ResultCache a =
    Dict String (ShrinkingResult a)


type alias Failures =
    Dict String Expectation


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


validatedFuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
validatedFuzzTest fuzzer desc getExpectation =
    let
        run seed runs =
            let
                -- Use a Dict so we don't report duplicate inputs.
                failures : Failures
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



{-
   (\seed runs ->
       getFailures Dict.empty seed runs Dict.empty

   )
       |> Test |> Labeled desc
-}


getFailures : Fuzzer a -> (a -> Expectation) -> Random.Seed -> Int -> Failures
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

        helper currentSeed remainingRuns results failures =
            let
                ( value, nextSeed ) =
                    Random.step genVal currentSeed
            in
                case Dict.get (toString value) results of
                    Just _ ->
                        -- we can skip this, already have the result in `failures`
                        failures

                    Nothing ->
                        let
                            ( newFailures, newResults ) =
                                findNewFailure fuzzer getExpectation failures results currentSeed value
                        in
                            if remainingRuns == 1 then
                                newFailures
                            else
                                helper nextSeed (remainingRuns - 1) newResults newFailures
    in
        helper initialSeed totalRuns Dict.empty Dict.empty


{-| Knowing that a value in not in the cache, determine if it causes the test to pass or fail.
-}
findNewFailure :
    Fuzzer a
    -> (a -> Expectation)
    -> Failures
    -> ResultCache a
    -> Random.Seed
    -> a
    -> ( Failures, ResultCache a )
findNewFailure fuzzer getExpectation failures results currentSeed value =
    case getExpectation value of
        Pass ->
            ( failures, Dict.insert (toString value) Passes results )

        failedExpectation ->
            let
                genTree =
                    unpackGenTree fuzzer

                ( rosetree, nextSeed_ ) =
                    Random.step genTree currentSeed

                ( failuresFromShrinking, resultsFromShrinking, minimalValue ) =
                    shrinkAndAdd rosetree getExpectation failedExpectation failures results
            in
                ( failuresFromShrinking
                , resultsFromShrinking
                  -- we don't have to insert value->minimalValue, shrinkAndAdd does that
                )


{-| Knowing that the rosetree's root already failed, but that it's not in the results cache, finds the shrunken failure.
Returns the updated failures dictionary, the updated results cache dictionary, and the shrunken failure itself.
-}
shrinkAndAdd :
    RoseTree a
    -> (a -> Expectation)
    -> Expectation
    -> Failures
    -> ResultCache a
    -> ( Failures, ResultCache a, a )
shrinkAndAdd rootTree getExpectation rootsExpectation failures results =
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
        ( Dict.insert (toString minimalValue) finalExpectation failures
        , Dict.insert (toString failingValue) (ShrinksTo minimalValue) newResults
        , minimalValue
        )


formatExpectation : ( String, Expectation ) -> Expectation
formatExpectation ( given, expectation ) =
    Test.Expectation.withGiven given expectation
