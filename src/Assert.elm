module Assert exposing (succeed, fail, isSuccess, formatFailures, addContext, toFailures, concatTests, equal, lessThan, greaterThan, Test)

{-| Making assertions.

@docs succeed, fail, isSuccess, formatFailures, addContext, toFailures, concatTests, equal, lessThan, greaterThan, Test
-}


{-| The Test from running a single Suite.
-}
type Test
    = Success
      -- TODO keep context around for success
    | Failure { messages : List String, context : List String }


{-| TODO: docs
-}
equal : { expected : a, actual : a } -> Test
equal { expected, actual } =
    if expected == actual then
        succeed
    else
        fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


{-| TODO: docs
-}
lessThan : { lesser : comparable, greater : comparable } -> Test
lessThan =
    greaterThan


{-| TODO: docs
-}
greaterThan : { lesser : comparable, greater : comparable } -> Test
greaterThan { lesser, greater } =
    if lesser < greater then
        succeed
    else
        fail ("Expected Greater: " ++ toString greater ++ "\nExpected Lesser:  " ++ toString lesser)


{-| TODO docs
-}
notEqual : a -> a -> Test
notEqual first second =
    if first == second then
        fail ("Expected different values, but both were:\n\n" ++ toString first)
    else
        succeed


{-| TODO docs
-}
fail : String -> Test
fail str =
    Failure { messages = [ str ], context = [] }


{-| TODO docs
-}
succeed : Test
succeed =
    Success


{-| In the event of success, returns [].
-}
toFailures : Test -> Maybe { messages : List String, context : List String }
toFailures test =
    case test of
        Success ->
            Nothing

        Failure record ->
            Just record


{-| TODO docs
-}
concatTests : List Test -> Test
concatTests =
    concatTestsHelp Success


{-| TODO docs
-}
isSuccess : Test -> Bool
isSuccess =
    (==) Success


concatTestsHelp : Test -> List Test -> Test
concatTestsHelp result tests =
    case tests of
        [] ->
            result

        Success :: rest ->
            concatTestsHelp result rest

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
                concatTestsHelp newFailure rest


{-| TODO docs
-}
addContext : String -> Test -> Test
addContext str test =
    case test of
        Success ->
            Success

        Failure record ->
            Failure { record | context = str :: record.context }


{-| TODO docs
-}
formatFailures : (String -> String) -> Test -> Test
formatFailures format test =
    case test of
        Failure record ->
            Failure { record | messages = List.map format record.messages }

        Success ->
            test
