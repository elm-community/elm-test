module ElmTest.Runner.Console where

import open ElmTest.Test
import ElmTest.Runner.String as RString

data IOState = Start
             | Printing String Bool
             | Exit Int

runDisplay : [Test] -> ConsoleSignals
runDisplay tests =
    let step msg st = case st of
          Exit _       -> st
          Printing m b -> Exit (if b then 0 else 1)
          Start        -> uncurry (flip Printing) <| RString.runDisplay tests
        state = foldp step Start (every millisecond)
    in { stdout = getPrints <~ state
       , exit   = getExit   <~ state
       }

getPrints : IOState -> String
getPrints st = case st of
  Start -> ""
  Printing s _ -> s
  Exit _ -> ""

getExit : IOState -> Maybe Int
getExit st = case st of
  Start -> Nothing
  Printing _ _ -> Nothing
  Exit c -> Just c

type ConsoleSignals = { stdout : Signal String
                      , exit   : Signal (Maybe Int)
                      }

