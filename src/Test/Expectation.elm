module Test.Expectation exposing (Expectation(..), Reason(..), withGiven)


type Expectation
    = Pass
    | Fail { given : String, description : String, reason : Reason }



-- TODO: given : Maybe String


type Reason
    = Custom
    | Equals String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff String String ( Int, String, String )
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }



--type alias CollectionDiff = {


withGiven : String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail { failure | given = newGiven }

        Pass ->
            expectation
