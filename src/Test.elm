module Test exposing (Test, failWith, toFailures, formatFailures, it, succeed, fail, concat)

{-| Testing

@docs Test, succeed, fail, it, failWith, toFailures, formatFailures, concat
-}


{-| TODO: docs
-}
failWith : String -> Test -> Test
failWith str =
    formatFailures (\_ -> str)


{-| TODO: docs
-}
it : String -> Test -> Test
it str test =
    case test of
        Success ->
            Success

        Failure record ->
            Failure { record | context = str :: record.context }


{-| The Test from running a single Suite.
-}
type Test
    = Success
    | Failure { messages : List String, context : List String }


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


{-| In the event of success, returns Nothing.
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
formatFailures : (String -> String) -> Test -> Test
formatFailures format test =
    case test of
        Failure record ->
            Failure { record | messages = List.map format record.messages }

        Success ->
            test


{-| TODO docs
-}
concat : List Test -> Test
concat =
    concatHelp Success



-- INTERNAL HELPERS --


concatHelp : Test -> List Test -> Test
concatHelp result tests =
    case tests of
        [] ->
            result

        Success :: rest ->
            concatHelp result rest

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
                concatHelp newFailure rest
