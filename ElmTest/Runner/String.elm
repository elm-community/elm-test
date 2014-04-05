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

pretty : String -> Result -> String
pretty name r = case r of
                    Pass     -> name ++ ": passed."
                    Fail msg -> name ++ ": FAILED. " ++ msg
                    Report r -> name ++ ": " ++ (vcat <| map (pretty name) r.results)

{-| Runs a list of tests. Returns the report as a String and True if all tests pass, False otherwise -}
runDisplay : Test -> (Bool, String)
runDisplay t =
    let result = run t
        r        = case result of
                      Report r -> r
                      Pass     -> {results = [Pass], passes = [Pass], failures = []}
                      Fail msg -> {results = [Fail msg], passes = [], failures = [Fail msg]}
        tests = case t of
                    TestCase n a -> [TestCase n a]
                    Suite n ts -> ts
        passedTests' = passedTests result
        passedSuites' = passedSuites result
        failedTests' = failedTests result
        failedSuites' = failedSuites result
        summary = vcat . map (indent 2) <| [
                    show (numberOfSuites t) ++ " suites run, containing " ++ show (numberOfTests t) ++ " tests"
                  , show passedSuites' ++ " suites passed, containing " ++ show passedTests' ++ " tests"
                  , show failedSuites' ++ " suites failed, containing " ++ show failedTests' ++ " failed tests"
                  ]
        --- TODO: implement results printing
        pass   = failedTests' == 0
        results = if pass
                  then []
                  else zipWith (pretty . nameOf) tests r.results
    in (pass, vcat <| summary :: results)
