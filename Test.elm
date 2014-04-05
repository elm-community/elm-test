module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

-- Example Usage
----------------
tests : [Test]
tests = [ (2^3) `equals` 1
        , 3 `equals` 3
        , defaultTest (assert True)
        , test "test head" (assertEqual 1 (head [1..10]))
        ]

tests2 : [Test]
tests2 = [ (2^3) `equals` 8
         , 3 `equals` 3
         , defaultTest (assert True)
         , test "test head" (assertEqual 1 (head [1..10]))
         ]

passingTest : Test
passingTest = test "passing test" <| assertEqual 0 0

failingTest : Test
failingTest = test "failing test" <| assertEqual 1 0

suite = Suite "Some tests" tests2
suite2 = Suite "A Test Suite" [suite, Suite "Some other tests" tests2, Suite "More tests!" tests2, 3 `equals` 3, Suite "Even more!!" tests2]
suite3 = Suite "A Test Suite" [suite, Suite "Some other tests" tests, Suite "More tests!" tests2, 3 `equals` 3, Suite "Even more!!" tests2]
