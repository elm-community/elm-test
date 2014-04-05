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

pretty : Int -> Result -> String
pretty n result =
    let passed = pass result
        msg = case result of
                Pass name     -> name ++ ": passed."
                Fail name msg -> name ++ ": FAILED. " ++ msg
                Report name r -> "Test Suite: " ++ name ++ ": "
                              ++ if passed then "all tests passed" else "FAILED\n"
                              ++ (vcat <| map (pretty (n + 2)) r.results)
    in  indent n msg

{-| Runs a list of tests. Returns the report as a String and True if all tests pass, False otherwise -}
runDisplay : Test -> (Bool, String)
runDisplay t =
    let result = run t
        tests = case t of
                    TestCase n a -> [TestCase n a]
                    Suite n ts -> ts
        passedTests' = passedTests result
        passedSuites' = passedSuites result
        failedTests' = failedTests result
        failedSuites' = failedSuites result
        summary = vcat . map (indent 2) <| [
                    show (numberOfSuites t) ++ " suites run, containing " ++ show (numberOfTests t) ++ " tests"
                  , if failedTests' == 0
                    then "All tests passed"
                    else show passedSuites' ++ " suites and " ++ show passedTests' ++ " tests passed"
                  , if failedTests' == 0
                    then ""
                    else show failedSuites' ++ " suites and " ++ show failedTests' ++ " tests failed"
                  ]
        --- TODO: implement results printing
        pass   = failedTests' == 0
        results' = if pass
                  then ""
                  else (pretty 0) result
    in (pass, vcat <| summary :: [results'])
