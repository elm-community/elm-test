module Test.Assertion exposing (Assertion(..))


type Assertion
    = Pass
    | Fail (List String)
