module ElmTest.Runner.String where

import open ElmTest.Run
import open ElmTest.Test

above : String -> String -> String
above s1 s2 = s1 ++ "\n" ++ s2

vcat : [String] -> String
vcat = foldr above ""

replicate : Int -> Char -> String
replicate n c = let go n = if n <= 0 
                           then []
                           else c :: go (n - 1)
                in String.fromList . go <| n

indent : Int -> String -> String
indent n = let indents = replicate n ' '
           in vcat . map (String.append indents) . String.lines


runDisplay : [Test] -> String
runDisplay ts = 
    let r = report ts
        passed = length r.passes
        failed = length r.failures
        summary = indent 2 . vcat <| [
                    show (length ts) ++ " tests executed"
                  , show passed    ++ " tests passed"
                  , show failed    ++ " tests failed"
                  ]
        --- TODO: implement results printing
        results = []
        
    in vcat <| summary :: results