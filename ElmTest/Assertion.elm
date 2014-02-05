module ElmTest.Assertion where

{-| The basic component of a test case, an assertion.

# Assert
@docs assertT, assert, assertEqual, assertNotEqual, (@=?), (@/=?)

-}    

-- The fundamental component of a Test Case, a thunk to be tested and associated metadata
data Assertion = AssertTrue     (() -> Bool)
               | AssertFalse    (() -> Bool)
               | AssertEqual    (() -> Bool) String String
               | AssertNotEqual (() -> Bool) String String

{-| Convenience operator for constructing an Assert Equals assertion.-}
(@=?) : a -> a -> Assertion
a @=? b = assertEqual a b

{-| Convenience operator for constructing an Assert Not Equals assertion. -}
(@/=?) : a -> a -> Assertion
a @/=? b = assertNotEqual a b

{-| Basic function to create an Assert True assertion. Delays execution until tests are run. -}
assertT : (() -> Bool) -> Assertion
assertT = AssertTrue

{-| Basic function to create an Assert True assertion. Delays execution until tests are run. -}          
assert : Bool -> Assertion
assert b = AssertTrue (\_ -> b)

{-| Basic function to create an Assert Equals assertion, the expected value goes on the left. -}
assertEqual : a -> a -> Assertion
assertEqual a b = AssertEqual (\_ -> a == b) (show a) (show b)

{-| Given a list of values and another list of expected values,
generate a list of Assert Equal assertions. -}
assertionList : [a] -> [a] -> [Assertion a]
assertionList xs ys = zipWith assertEqual xs ys

{- Basic function to create an Assert Not Equals assertion. -}
assertNotEqual : a -> a -> Assertion
assertNotEqual a b = AssertNotEqual (\_ -> a /= b) (show a) (show b)
