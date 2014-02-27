module ElmTest.Runner.Console (runDisplay) where

{-| Run a test suite as a command-line script. 

# Run
@docs runDisplay

-}

import IO.IO as IO
    
import open ElmTest.Test
import ElmTest.Runner.String as RString

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
runDisplay : [Test] -> IO.IO ()
runDisplay tests = IO.putStrLn . snd . RString.runDisplay <| tests
