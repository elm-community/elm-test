module Test.Runner.Console exposing (plain)

{-| Run a test suite as a command-line script.

# Run
@docs run, plain

-}

import Test exposing (Test)
import Test.Runner.Console.Plain as Plain
import Random.Pcg as Random
import Html.App as Html
import Html


{-| Run the tests single-threaded with unformatted output. Does not require
hooking up ports. Always uses initial random seed 42.
-}
plain : Test -> Program Never
plain test =
    let
        results =
            Test.runWithSeed (Random.initialSeed 42) test
    in
        Html.beginnerProgram
            { model = Plain.summaryLines results |> Plain.logSummaryLines
            , view = (\_ -> Html.text "")
            , update = (\_ _ -> ())
            }
