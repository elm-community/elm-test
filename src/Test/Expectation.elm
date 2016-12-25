module Test.Expectation exposing (Expectation(..), withGiven)


type Expectation
    = Pass
    | Pending
    | Fail { given : String, message : String }


withGiven : String -> Expectation -> Expectation
withGiven given outcome =
    case outcome of
        Fail { message } ->
            Fail { given = given, message = message }

        Pending ->
            outcome

        Pass ->
            outcome
