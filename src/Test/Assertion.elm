module Test.Assertion exposing (Assertion(..), formatFailure)


type Assertion
    = Pass
    | Fail String


formatFailure : (String -> String) -> Assertion -> Assertion
formatFailure format outcome =
    case outcome of
        Fail message ->
            Fail (format message)

        Pass ->
            outcome
