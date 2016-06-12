module Test.Runner.Log exposing (run)

import Html
import Html.App
import Random.Pcg as Random
import Test exposing (Test, Suite)
import Test.Runner.String


run : Suite -> Program Never
run suite =
    Test.Runner.String.run suite
        |> logOutput


runWithOptions : Random.Seed -> Int -> Suite -> Program Never
runWithOptions seed runs suite =
    Test.Runner.String.runWithOptions seed runs suite
        |> logOutput


logOutput : ( String, Int ) -> Program Never
logOutput ( output, failureCount ) =
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
        Html.App.beginnerProgram
            { model = ()
            , update = \_ model -> model
            , view = \_ -> Html.text ""
            }
