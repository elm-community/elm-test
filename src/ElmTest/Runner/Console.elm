module ElmTest.Runner.Console exposing (runDisplay, runSuite) -- where

{-| Run a test suite as a command-line script.

# Run
@docs runDisplay

-}

import List
import String
import ElmTest.Test exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.String as String
import Html.App as Html
import Html

{-| Run a test and print the output
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
                  Debug.log out ""

                False ->
                  Debug.crash out

    _ ->
      Debug.crash ""


runSuite : Test -> Program Never
runSuite consoleTests =
  Html.beginnerProgram
    { model = runDisplay consoleTests
    , view = (\x -> Html.text "")
    , update = (\x y -> y)
    }
