module Main where

import ElmTest.Runner.Console (runDisplay)
import open ElmTest.Test
import ElmTest.Run as R
import ElmTest.Assertion as A

tests : [Test]
tests = [ (R.run (0 `equals` 0)) `equals` Nothing
        , test "pass" <| A.assert (R.pass Nothing)
        , test "fail" <| A.assertNotEqual (R.fail Nothing) True
        ] ++ (map defaultTest <| A.assertionList [1..10] [1..10])

console = runDisplay tests