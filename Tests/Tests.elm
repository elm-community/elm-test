module Main where

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console (runDisplay)
import ElmTest.Test (..)

tests : [Test]
tests = [ (R.run (0 `equals` 0)) `equals` R.Pass
        , test "pass" <| A.assert (R.pass R.Pass)
        , test "fail" <| A.assertNotEqual (R.fail R.Pass) True
        ] ++ (map defaultTest <| A.assertionList [1..10] [1..10])

console = runDisplay <| Suite "All Tests" tests
