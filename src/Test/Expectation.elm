module Test.Expectation exposing (Expectation(..), withGiven)


type Expectation
    = Pass
    | Fail String String


withGiven : String -> Expectation -> Expectation
withGiven given outcome =
    case outcome of
        Fail _ message ->
            Fail given message

        Pass ->
            outcome
