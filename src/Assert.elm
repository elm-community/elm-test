module Assert exposing (Outcome, succeed, fail, formatError, addContext, toFailures, concatOutcomes, withoutSuccesses, equal, Assertion, assert, assertFuzz, resolve)

import Random.Pcg as Random


{-| The outcome from running a single test.
-}
type Outcome
    = Success
      -- TODO add (List String) to Success to keep context around
    | Failure { messages : List String, context : List String }


{-| TODO: docs
-}
type Assertion
    = Assertion (Random.Seed -> Int -> Bool -> Outcome)



-- TODO Assertion (Maybe String -> Outcome)


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
    Failure { messages = [ str ], context = [] }


succeed : Outcome
succeed =
    Success


{-| In the event of success, returns [].
-}
toFailures : Outcome -> Maybe { messages : List String, context : List String }
toFailures outcome =
    case outcome of
        Success ->
            Nothing

        Failure record ->
            Just record


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

        ((Failure record) as currentFailure) :: rest ->
            let
                newFailure =
                    case result of
                        Failure { messages } ->
                            -- NOTE: we use the first context we get, and
                            -- assume all other contexts are the same.
                            Failure { record | messages = record.messages ++ messages }

                        Success ->
                            currentFailure
            in
                concatOutcomesHelp newFailure rest


addContext : String -> Outcome -> Outcome
addContext str outcome =
    case outcome of
        Success ->
            Success

        Failure record ->
            Failure { record | context = str :: record.context }


formatError : String -> Outcome -> Outcome
formatError str outcome =
    case outcome of
        Failure record ->
            Failure { record | messages = List.map (\_ -> str) record.messages }

        Success ->
            outcome
