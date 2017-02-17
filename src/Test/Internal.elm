module Test.Internal exposing (Test(..), fuzzTest, filter)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Dict exposing (Dict)
import Shrink exposing (Shrinker)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal exposing (unpackGenVal, unpackGenTree)
import RoseTree exposing (RoseTree(..))
import Lazy.List


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


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


fuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest fuzzer desc getExpectation =
    {- Fuzz test algorithm with opt-in RoseTrees:
       Generate a single value by passing the fuzzer True (indicates skip shrinking)
       Run the test on that value. If it fails:
           Generate the rosetree by passing the fuzzer False *and the same random seed*
           Find the new failure by looking at the children for any shrunken values:
               If a shrunken value causes a failure, recurse on its children
               If no shrunken value replicates the failure, use the root
       Whether it passes or fails, do this n times
    -}
    let
        getFailures failures currentSeed remainingRuns results =
            let
                genVal =
                    unpackGenVal fuzzer

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
                                case getExpectation value of
                                    Pass ->
                                        ( failures
                                        , Dict.insert (toString value) Passes results
                                        )

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
                        in
                            if remainingRuns == 1 then
                                newFailures
                            else
                                getFailures newFailures nextSeed (remainingRuns - 1) newResults

        run seed runs =
            let
                -- Use a Dict so we don't report duplicate inputs.
                failures : Dict String Expectation
                failures =
                    getFailures Dict.empty seed runs Dict.empty
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


shrinkAndAdd :
    RoseTree a
    -> (a -> Expectation)
    -> Expectation
    -> Dict String Expectation
    -> Dict String (ShrinkingResult a)
    -> ( Dict String Expectation, Dict String (ShrinkingResult a), a )
shrinkAndAdd rootTree getExpectation rootsExpectation failures results =
    -- Knowing that the root already failed, adds the shrunken failure to the dictionary
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


isFail : Expectation -> Bool
isFail =
    (/=) Pass
