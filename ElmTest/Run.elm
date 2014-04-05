module ElmTest.Run where

{-| Basic utilities for running tests and customizing the output. If you don't care about customizing
the output, instead look at the ```runDisplay``` series in ElmTest.Runner

# Run
@docs run, report, pass, fail

-}

import ElmTest.Assertion (..)
import ElmTest.Test (..)

data Result = Pass | Fail String | Report { results  : [Result]
                                          , passes   : [Result]
                                          , failures : [Result]
                                          }

{-| Run a test and get a Result -}
run : Test -> Result
run test =
    case test of
        TestCase _ assertion -> let runAssertion t m = if t ()
                                                       then Pass
                                                       else Fail m
                                in case assertion of
                                     AssertEqual t a b    -> runAssertion t <| "Expected: " ++ a ++ "; got: " ++ b
                                     AssertNotEqual t a b -> runAssertion t <| a ++ " equals " ++ b
                                     AssertTrue  t        -> runAssertion t <| "not True"
                                     AssertFalse t        -> runAssertion t <| "not False"
        Suite _ tests -> let results = map run tests
                             (passes, fails) = partition pass results
                         in Report { results  = results
                                   , passes   = passes
                                   , failures = fails
                                   }

{-| Transform a Result into a Bool. True if the result represents a pass, otherwise False -}
pass : Result -> Bool
pass m = case m of
           Pass     -> True
           Fail _   -> False
           Report r -> if (length (.failures r) > 0) then False else True

{-| Transform a Result into a Bool. True if the result represents a fail, otherwise False -}
fail : Result -> Bool
fail = not . pass

passedTests : Result -> Int
passedTests result = case result of
                        Pass -> 1
                        Fail _ -> 0
                        Report r -> sum . map passedTests <| r.results

failedTests : Result -> Int
failedTests result = case result of
                        Pass -> 0
                        Fail _ -> 1
                        Report r -> sum . map failedTests <| r.results

passedSuites : Result -> Int
passedSuites result = case result of
                        Report r -> let passed = if length r.failures == 0
                                                 then 1
                                                 else 0
                                    in  passed + (sum . map passedSuites <| r.results)
                        _ -> 0

failedSuites : Result -> Int
failedSuites result = case result of
                        Report r -> let failed = if length r.failures > 0
                                                 then 1
                                                 else 0
                                    in  failed + (sum . map failedSuites <| r.results)
                        _ -> 0
