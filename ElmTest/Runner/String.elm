module ElmTest.Runner.String (runDisplay) where

{-| Run a test suite and display it as a string.

# Run
@docs runDisplay

-}
    
import ElmTest.Run (..)
import ElmTest.Test (..)

-- | Some pretty printing stuff. Should be factored into a pretty printing library.    
vcat : [String] -> String
vcat = concat . intersperse "\n"

replicate : Int -> Char -> String
replicate n c = let go n = if n <= 0 
                           then []
                           else c :: go (n - 1)
                in String.fromList . go <| n

indent : Int -> String -> String
indent n = let indents = replicate n ' '
           in vcat . map (String.append indents) . String.lines

pretty : Test -> Result -> String
pretty (TestCase name _) r = 
    case r of
      Nothing  -> name ++ ": passed."
      Just msg -> name ++ ": FAILED. " ++ msg

{-| Runs a list of tests. Returns the report as a String and True if all tests pass, False otherwise -}
runDisplay : [Test] -> (Bool, String)
runDisplay ts = 
    let r = report ts
        passed = length r.passes
        failed = length r.failures
        summary = vcat . map (indent 2) <| [
                    show (length ts) ++ " tests executed"
                  , show passed    ++ " tests passed"
                  , show failed    ++ " tests failed"
                  ]
        --- TODO: implement results printing
        pass   = failed == 0
        results = if pass
                  then []
                  else zipWith pretty ts r.results
    in (pass, vcat <| summary :: results)
