module Runner.Log exposing (run, runWithOptions)

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
import Runner.String exposing (Summary)
import String


{-| Run the test using the default `Test.Runner.String` options.
-}
run : Test -> a -> a
run test =
    Runner.String.run test
        |> logOutput


{-| Run the test using the provided options.
-}
runWithOptions : Int -> Random.Seed -> Test -> a -> a
runWithOptions runs seed test =
    Runner.String.runWithOptions runs seed test
        |> logOutput


summarize : Summary -> String
summarize { output, passed, failed } =
    let
        headline =
            if failed > 0 then
                output ++ "\n\nTEST RUN FAILED"
            else
                "TEST RUN PASSED"
    in
        String.join "\n"
            [ output
            , headline ++ "\n"
            , "Passed: " ++ toString passed
            , "Failed: " ++ toString failed
            ]


logOutput : Summary -> a -> a
logOutput summary arg =
    let
        output =
            summarize summary ++ "\n\nExit code"

        _ =
            if summary.failed > 0 then
                output
                    |> (flip Debug.log 1)
                    |> (\_ -> Debug.crash "FAILED TEST RUN")
                    |> (\_ -> ())
            else
                output
                    |> (flip Debug.log 0)
                    |> (\_ -> ())
    in
        arg
