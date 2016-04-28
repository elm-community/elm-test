module ElmTest.Runner.Console exposing (runDisplay)

{-| Run a test suite as a command-line script.

# Run
@docs runDisplay

-}

import List
import String
import ElmTest.Test exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.String as String


{-| Run a list of tests in the IO type from [Max New's Elm IO library](https://github.com/maxsnew/IO/).
Requires this library to work. Results are printed to console once all tests have completed. Exits with
exit code 0 if all tests pass, or with code 1 if any tests fail.
-}
runDisplay : Test -> String
runDisplay tests =
  case String.run tests of
    ( summary, allPassed ) :: results ->
      let
        out =
          summary ++ "\n\n" ++ (String.concat << List.intersperse "\n" << List.map fst <| results)
      in
        case Run.pass allPassed of
                True ->
                  Debug.log "" out

                False ->
                  Debug.crash out

    _ ->
      Debug.crash ""
