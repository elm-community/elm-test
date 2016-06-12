module Test.Outcome exposing (Outcome, pass, fail, toFailures, concat, formatFailure)

{-| Functions for working with test outcomes.
-}


{-| the result of a single test run. this can either be a [`pass`](#pass) or
[`fail`](#fail).

use [`tofailures`](#tofailures) to convert an `outcome` into appropriately
contextualized failure messages.
-}
type Outcome
    = Pass
    | Fail (List String)


{-| A [`Test`](#Test) which failed with the given message.

-- TODO code sample
-}
fail : String -> Outcome
fail str =
    Fail [ str ]


{-| A [`Test`](#Test) which passed.

-- TODO code sample
-}
pass : Outcome
pass =
    Pass


{-| Return contextualized failure messages from the given [`Test`](#Test).

Note that fuzz tests may return multiple failure messages from a single `Test`!

-- TODO code sample
-}
toFailures : Outcome -> Maybe (List String)
toFailures outcome =
    case outcome of
        Pass ->
            Nothing

        Fail failures ->
            Just failures


{-| Format all the failure messages in a given `Test`.

-- TODO code sample
-}
formatFailure : (String -> String) -> Outcome -> Outcome
formatFailure format outcome =
    case outcome of
        Fail messages ->
            messages
                |> List.map format
                |> Fail

        Pass ->
            outcome


{-| TODO document
-}
concat : List Outcome -> Outcome
concat =
    concatHelp pass


concatHelp : Outcome -> List Outcome -> Outcome
concatHelp result outcomes =
    case outcomes of
        [] ->
            result

        Pass :: rest ->
            case result of
                Pass ->
                    concatHelp result rest

                (Fail _) as failure ->
                    concatHelp failure rest

        ((Fail newFailures) as first) :: rest ->
            case result of
                Pass ->
                    concatHelp first rest

                Fail oldFailures ->
                    concatHelp (Fail (oldFailures ++ newFailures)) rest
