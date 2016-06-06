module Assert exposing (succeed, fail, formatFailures, addContext, toFailures, concatOutcomes, withoutSuccesses, equal, Outcome)

{-| Making assertions.
-}


{-| The outcome from running a single test.
-}
type Outcome
    = Success
      -- TODO keep context around for success
    | Failure { messages : List String, context : List String }


{-| TODO: docs
-}
equal : { expected : a, actual : a } -> Outcome
equal { expected, actual } =
    if expected == actual then
        succeed
    else
        fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


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


formatFailures : (String -> String) -> Outcome -> Outcome
formatFailures format outcome =
    case outcome of
        Failure record ->
            Failure { record | messages = List.map format record.messages }

        Success ->
            outcome
