module Test where

import open ElmTest.Assertion
import open ElmTest.Test

-- Example Usage
----------------
tests : [Test]
tests = [ (2^3) ~=? 1
        , 3 ~=? 3
        , defaultTest (assert True)
        , test "test head" (assertEqual 1 (head [1..10]))
        ]
       -- (testFunction "Square"
       --                                 (\x -> if (x < 4) then x^2 else x^3)
       --                                 (zip [1..5] [1,4,9,16,25]))
