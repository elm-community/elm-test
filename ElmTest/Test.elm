module ElmTest.Test where

{-| The units of a test suite, named tests.

# Test
@docs test, equals, defaultTest

-}

import ElmTest.Assertion (..)

data Test = TestCase String Assertion | Suite String [Test]

nameOf : Test -> String
nameOf test = case test of
                TestCase n _ -> n
                Suite    n _ -> n

numberOf : Test -> Int
numberOf test = case test of
                    TestCase _ _  -> 1
                    Suite    _ ts -> sum . map numberOf <| ts

{-| Convenience function for quickly constructing Assert Equals tests. -}
equals : a -> a -> Test
equals a b = defaultTest <| assertEqual a b

{-| Basic function to create a Test Case -}
test : String -> Assertion -> Test
test name a = TestCase name a

{-| Automatically determines a name for the created test (use this if you're lazy). -}
defaultTest : Assertion -> Test
defaultTest a =
    let name = case a of
                 AssertTrue _ -> "True"
                 AssertTrue _ -> "False"
                 AssertEqual _ a b    -> a ++ " == " ++ b
                 AssertNotEqual _ a b -> a ++ " /= " ++ b
    in test name a
