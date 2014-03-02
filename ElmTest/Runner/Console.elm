module ElmTest.Runner.Console (runDisplay) where

{-| Run a test suite as a command-line script. 

# Run
@docs runDisplay

-}

import open as IO
    
import open ElmTest.Test
import ElmTest.Runner.String as RString

{-| Run a list of tests in the IO type from [Max New's Elm IO library](https://github.com/maxsnew/IO/).
Requires this library to work. Results are printed to console once all tests have completed.
-}               
runDisplay : [Test] -> IO ()
runDisplay tests = putStrLn . snd . RString.runDisplay <| tests
