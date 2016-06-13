module Test.Runner.Log exposing (run, runWithOptions)

{-| # Log Runner

@docs run, runWithOptions
-}

import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner.String


{-| TODO document
-}
run : Test -> a -> a
run test =
    Test.Runner.String.run test
        |> logOutput


{-| TODO document
-}
runWithOptions : Random.Seed -> Int -> Test -> a -> a
runWithOptions seed runs test =
    Test.Runner.String.runWithOptions seed runs test
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
