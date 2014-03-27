module ElmTest.Run where

{-| Basic utilities for running tests and customizing the output. If you don't care about customizing
the output, instead look at the ```runDisplay``` series in ElmTest.Runner

# Run
@docs run, report, pass, fail

-}  

import ElmTest.Assertion (..)
import ElmTest.Test (..)

type Result = Maybe String
type Report = { results : [Result]
              , passes : [Result]
              , failures : [Result] }

{-| Run a test and get a Result -}
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

{-| Transform a Result into a Bool. True if the result represents a pass, otherwise False -}
pass : Result -> Bool
pass m = case m of
           Nothing -> True
           Just _  -> False

{-| Transform a Result into a Bool. True if the result represents a fail, otherwise False -}
fail : Result -> Bool
fail = not . pass

{-| Run a list of tests and get a Report -}
report : [Test] -> Report
report ts = let results         = map run ts
                (passes, fails) = partition pass results
            in { results = results
               , passes  = passes
               , failures = fails
               }