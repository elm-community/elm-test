module Assert exposing (Outcome, succeed, fail, formatError, addContext, toFailures, concatOutcomes, withoutSuccesses, equal, Assertion, assert, assertFuzz, resolve)

import Random.Pcg as Random


{-| The outcome from running a single test.
-}
type Outcome
    = Success
      -- TODO add (List String) to Success to keep context around
    | Failure String (List String)


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
    Failure str []


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

        Failure msg context ->
            context ++ [ msg ]


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

        ((Failure msg context) as currentFailure) :: rest ->
            let
                newFailure =
                    case result of
                        Failure otherMsg otherContext ->
                            -- TODO this seems broken - why treat otherMsg like context?
                            Failure msg (context ++ (otherMsg :: otherContext))

                        Success ->
                            currentFailure
            in
                concatOutcomesHelp newFailure rest


addContext : String -> Outcome -> Outcome
addContext str outcome =
    case outcome of
        Success ->
            Success

        Failure msg context ->
            Failure msg (str :: context)


formatError : String -> Outcome -> Outcome
formatError str outcome =
    case outcome of
        Failure msg context ->
            Failure str context

        Success ->
            outcome
