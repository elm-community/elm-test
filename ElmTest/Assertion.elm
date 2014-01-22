module ElmTest.Assertion where

-- The fundamental component of a Test Case, a thunk to be tested and associated metadata
data Assertion = AssertTrue     (() -> Bool)
               | AssertFalse    (() -> Bool)
               | AssertEqual    (() -> Bool) String String
               | AssertNotEqual (() -> Bool) String String

{-|  Convenience operator for constructing an Assert Equals assertion.-}
(@=?) : a -> a -> Assertion
a @=? b = assertEqual a b

-- Convenience operator for constructing an Assert Not Equals assertion
(@/=?) : a -> a -> Assertion
a @/=? b = assertNotEqual a b

-- Basic function to create an Assert True assertion
assertT : (() -> Bool) -> Assertion
assertT = AssertTrue

assert : Bool -> Assertion
assert b = AssertTrue (\_ -> b)

-- Basic function to create an Assert Equals assertion, the expected value goes on the left
assertEqual : a -> a -> Assertion
assertEqual a b = AssertEqual (\_ -> a == b) (show a) (show b)

-- | Don't see the value of this
-- -- Given a list of values and another list of expected values, generate a list of
-- -- Assert Equal assertions
-- assertionList : [a] -> [a] -> [Assertion a]
-- assertionList xs ys = map (\(a, b) -> assertEqual a b) <| zip xs ys

-- Basic function to create an Assert Not Equals assertion
assertNotEqual : a -> a -> Assertion
assertNotEqual a b = AssertNotEqual (\_ -> a /= b) (show a) (show b)
