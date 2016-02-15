module ElmTest.Run (Result(..), run, pass, fail, failedTests, passedTests, failedSuites, passedSuites) where

{-| Basic utilities for running tests and customizing the output. If you don't care about customizing
the output, instead look at the `runDisplay` series in ElmTest.Runner

# Run
@docs Result, run, pass, fail

# Undocumented
@docs failedTests, passedTests, failedSuites, passedSuites
-}

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)
import List


type alias Summary =
  { results : List Result
  , passes : List Result
  , failures : List Result
  }


{-| Test result
-}
type Result
  = Pass String
  | Fail String String
  | Report String Summary


{-| Run a test and get a Result
-}
run : Test -> Result
run test =
  case test of
    TestCase name assertion ->
      let
        runAssertion t m =
          if t () then
            Pass name
          else
            Fail name m
      in
        case assertion of
          AssertEqual t a b ->
            runAssertion t
              <| "Expected: "
              ++ a
              ++ "; got: "
              ++ b

          AssertNotEqual t a b ->
            runAssertion t <| a ++ " equals " ++ b

          AssertTrue t ->
            runAssertion t "not True"

          AssertFalse t ->
            runAssertion t "not False"

          AlwaysPass ->
            runAssertion (always True) ""

          AlwaysFail s ->
            runAssertion (always False) s

    Suite name tests ->
      let
        results =
          List.map run tests

        ( passes, fails ) =
          List.partition pass results
      in
        Report
          name
          { results = results
          , passes = passes
          , failures = fails
          }


{-| Transform a Result into a Bool. True if the result represents a pass, otherwise False
-}
pass : Result -> Bool
pass m =
  case m of
    Pass _ ->
      True

    Fail _ _ ->
      False

    Report _ r ->
      if (List.length (.failures r) > 0) then
        False
      else
        True


{-| Transform a Result into a Bool. True if the result represents a fail, otherwise False
-}
fail : Result -> Bool
fail =
  not << pass


{-| Determine the number of Tests that passed
-}
passedTests : Result -> Int
passedTests result =
  case result of
    Pass _ ->
      1

    Fail _ _ ->
      0

    Report n r ->
      List.sum << List.map passedTests <| r.results


{-| Determine the number of Tests that failed
-}
failedTests : Result -> Int
failedTests result =
  case result of
    Pass _ ->
      0

    Fail _ _ ->
      1

    Report n r ->
      List.sum << List.map failedTests <| r.results


{-| Determine the number of Suites that passed
-}
passedSuites : Result -> Int
passedSuites result =
  case result of
    Report n r ->
      let
        passed =
          if List.length r.failures == 0 then
            1
          else
            0
      in
        passed + (List.sum << List.map passedSuites <| r.results)

    _ ->
      0


{-| Determine the number of Suites that failed
-}
failedSuites : Result -> Int
failedSuites result =
  case result of
    Report n r ->
      let
        failed =
          if List.length r.failures > 0 then
            1
          else
            0
      in
        failed + (List.sum << List.map failedSuites <| r.results)

    _ ->
      0
