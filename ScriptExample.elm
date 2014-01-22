module ScriptExample where

import Test
import ElmTest.Runner.String as String

port stdout : Signal String
port stdout =
  let interpret st = case st of
        Start -> ""
        Printing s _ -> s
        Exit _ -> ""
  in interpret <~ runTests

port exit : Signal (Maybe Int)
port exit = let interpret st = case st of
                  Start -> Nothing
                  Printing _ _ -> Nothing
                  Exit c -> Just c
            in interpret <~ runTests

data IOState = Start
             | Printing String Bool
             | Exit Int

runTests = runner (every millisecond)

runner : Signal a -> Signal IOState
runner = foldp step Start

step : a -> IOState -> IOState
step msg st = case st of
  Exit _       -> st
  Printing m b -> Exit (if b then 0 else 1)
  Start        -> uncurry (flip Printing) <| String.runDisplay Test.tests
