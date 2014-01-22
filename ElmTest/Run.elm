module ElmTest.Run where

import open ElmTest.Assertion
import open ElmTest.Test

-- Results
----------

-- The type representing a test result. Nothing denotes "Pass" and Just string
-- contains the error message in the event of a failure
type Result = Maybe String
type Report = { results : [Result]
              , passes : [Result]
              , failures : [Result] }

-- Function to run a test and get a result
run : Test -> Result
run (TestCase _ assertion) = 
    let runAssertion t m = if t ()
                           then Nothing 
                           else Just m
    in case assertion of
         AssertEqual t a b    -> runAssertion t <| "Expected: " ++ a ++ "; got: " ++ b
         AssertNotEqual t a b -> runAssertion t <| a ++ " equals " ++ b
         AssertTrue  t        -> runAssertion t <| "not True"
         AssertFalse t        -> runAssertion t <| "not False"

pass : Result -> Bool
pass m = case m of
           Nothing -> True
           Just _  -> False

fail : Result -> Bool
fail = not . pass

report : [Test] -> Report
report ts = let results         = map run ts
                (passes, fails) = partition pass results
            in { results = results
               , passes  = passes
               , failures = fails
               }