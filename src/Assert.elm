module Assert exposing (Outcome, succeed, fail, formatError, toFailures, concatOutcomes, withoutSuccesses, equal, Assertion, assert, assertFuzz, resolve)

import Random.Pcg as Random


{-| The outcome from running a single test.
-}
type Outcome
    = Success
    | Failure (List String)


{-| TODO: docs
-}
type Assertion
    = Assertion (Random.Seed -> Int -> Bool -> Outcome)


{-| TODO: docs
-}
equal : { expected : a, actual : a } -> Assertion
equal { expected, actual } =
    let
        run _ _ _ =
            if expected == actual then
                succeed
            else
                fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)
    in
        Assertion run


assert : Outcome -> Assertion
assert outcome =
    Assertion (\_ _ _ -> outcome)


assertFuzz : (Random.Seed -> Int -> Bool -> Outcome) -> Assertion
assertFuzz =
    Assertion


resolve : Random.Seed -> Int -> Bool -> Assertion -> Outcome
resolve seed runs doShrink (Assertion run) =
    run seed runs doShrink


fail : String -> Outcome
fail str =
    Failure [ str ]


succeed : Outcome
succeed =
    Success


{-| In the event of success, returns [].
-}
toFailures : Outcome -> List String
toFailures outcome =
    case outcome of
        Success ->
            []

        Failure failures ->
            failures


withoutSuccesses : List Outcome -> List Outcome
withoutSuccesses =
    List.filter ((/=) Success)


concatOutcomes : List Outcome -> Outcome
concatOutcomes =
    concatOutcomesHelp Success


concatOutcomesHelp : Outcome -> List Outcome -> Outcome
concatOutcomesHelp result outcomes =
    case outcomes of
        [] ->
            result

        Success :: rest ->
            concatOutcomesHelp result rest

        (Failure messages) :: rest ->
            let
                totalMessages =
                    case result of
                        Failure otherMessages ->
                            messages ++ otherMessages

                        Success ->
                            messages
            in
                concatOutcomesHelp (Failure totalMessages) rest


formatError : (List String -> List String) -> Outcome -> Outcome
formatError format outcome =
    case outcome of
        Success ->
            Success

        Failure messages ->
            Failure (format messages)
