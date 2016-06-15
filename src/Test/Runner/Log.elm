module Test.Runner.Log exposing (run, runWithOptions)

{-| # Log Runner

Runs a test and outputs its results using `Debug.log`, then calls `Debug.crash`
if there are any failures.

This is not the prettiest runner, but it is simple and cross-platform. For
example, you can use it as a crude Node runner like so:

    $ elm-make LogRunnerExample.elm --output=elm.js
    $ node elm.js

This will log the test results to the console, then exit with exit code 0
if the tests all passed, and 1 if any failed.

@docs run, runWithOptions
-}

import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner.String


{-| Run the test using the default `Test.Runner.String` options.
-}
run : Test -> a -> a
run test =
    Test.Runner.String.run test
        |> logOutput


{-| Run the test using the provided options.
-}
runWithOptions : Int -> Random.Seed -> Test -> a -> a
runWithOptions runs seed test =
    Test.Runner.String.runWithOptions runs seed test
        |> logOutput


logOutput : ( String, Int ) -> a -> a
logOutput ( output, failureCount ) arg =
    let
        _ =
            if failureCount > 0 then
                (output ++ "\n\n\n" ++ toString failureCount ++ " TESTS FAILED!\n\nExit code")
                    |> (flip Debug.log 1)
                    |> (\_ -> Debug.crash "FAILED TEST RUN")
                    |> (\_ -> ())
            else
                (output ++ "\n\n\nALL TESTS PASSED!\n\nExit code")
                    |> (flip Debug.log 0)
                    |> (\_ -> ())
    in
        arg
