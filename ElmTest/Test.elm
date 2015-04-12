module ElmTest.Test where

{-| The units of a test suite, named tests.

# Test
@docs test, equals, defaultTest, suite

-}

import ElmTest.Assertion exposing (..)
import List

type Test = TestCase String Assertion | Suite String (List Test)

nameOf : Test -> String
nameOf test = case test of
                TestCase n _ -> n
                Suite    n _ -> n

numberOfTests : Test -> Int
numberOfTests test = case test of
                        TestCase _ _  -> 1
                        Suite    _ ts -> List.sum << List.map numberOfTests <| ts

numberOfSuites : Test -> Int
numberOfSuites test = case test of
                        TestCase _ _  -> 0
                        Suite    _ ts -> 1 + (List.sum << List.map numberOfSuites <| ts)

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

{-| Convert a list of `Test`s to a `Suite`. Test suites are used to group tests into
logical units, simplifying the management and running of many tests. The `String` is the
name of the `Suite`.  -}
suite : String -> List Test -> Test
suite = Suite
