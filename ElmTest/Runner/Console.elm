module ElmTest.Runner.Console (runDisplay) where

{-| Run a test suite as a command-line script. 

# Run
@docs runDisplay

-}
    
import open ElmTest.Test
import ElmTest.Runner.String as RString

data IOState = Start
             | Printing String Bool
             | Exit Int

{-| Run a test suite, printing results to the stdout signal and then signalling exit.
Returns a 0 exit code if the test suite passed and a 1 exit code otherwise.

The output signals here should be hooked up to your stdout and exit
ports. Elm supports has a built-in handler for a stdout port but
you'll need to define one for exit. The following implementation will
work with this exit signal provided:
```javascript
var exit = function(code) {
  if (!(code === null)) {
     process.exit(code);
  }
}
```
-}               
runDisplay : [Test] -> { stdout : Signal String
                       , exit   : Signal (Maybe Int)
                       }
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
