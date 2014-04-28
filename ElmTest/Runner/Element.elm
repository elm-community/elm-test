module ElmTest.Runner.Element (runDisplay) where

{-| Run a test suite and display it as an Element

# Run
@docs runDisplay

-}

import ElmTest.Run as Run
import ElmTest.Test (..)
import ElmTest.Runner.String as String

-- Given a result, render it in plainText and return a pass/fail color
pretty : (String, Run.Result) -> Element
pretty (s, m) =
    case m of
      Run.Pass _  -> color green . plainText <| s
      Run.Fail _ msg -> color red .   plainText <| s
      Run.Report _ {results, passes, failures} -> color purple <| plainText s

{-| Runs a list of tests and renders the results as an Element -}
runDisplay : Test -> Element
runDisplay tests =
    let ((summary, allPassed) :: results) = String.run tests
    in  flow down <| map pretty results
