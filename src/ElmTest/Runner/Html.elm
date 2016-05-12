module ElmTest.Runner.Html exposing (runSuite) -- where

{-| Run a test suite as an Html Program

# Run
@docs runSuite

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
        out
    _ ->
      Debug.crash ""


{-| Run the test suite and display as Html.
-}
runSuite : Test -> Program Never
runSuite consoleTests =
  Html.beginnerProgram
    { model = runDisplay consoleTests
    , view = (\x -> Html.pre [] [Html.text x])
    , update = (\x y -> x)
    }
