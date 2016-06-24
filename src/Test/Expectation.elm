module Test.Expectation exposing (Expectation(..), formatFailure)


type Expectation
    = Pass
    | Fail String


formatFailure : (String -> String) -> Expectation -> Expectation
formatFailure format outcome =
    case outcome of
        Fail message ->
            Fail (format message)

        Pass ->
            outcome
