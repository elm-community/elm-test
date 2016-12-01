module Test.Expectation exposing (Expectation(..), Reason(..), withGiven)


type Expectation
    = Pass
    | Fail { given : String, description : String, reason : Reason }



-- TODO: given : Maybe String


type Reason
    = Custom
    | Equals String String
    | Comparison String String


withGiven : String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail { failure | given = newGiven }

        Pass ->
            expectation
