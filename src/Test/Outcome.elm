module Test.Outcome exposing (Test, Outcome, failWith, toFailures, formatFailures, it, pass, fail)

{-| Testing

@docs Outcome, pass, fail, it, failWith, toFailures, formatFailures
-}

import Random.Pcg as Random exposing (Generator)


type Test
    = Test (Random.Seed -> Int -> Bool -> List Outcome)


{-| The result of a single test run. This can either be a [`pass`](#pass) or
[`fail`](#fail).

Use [`toFailures`](#toFailures) to convert an `Outcome` into appropriately
contextualized failure messages.
-}
type Outcome
    = Pass
    | Fail { messages : List String, context : List String }


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (failWith)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> failWith "thought they'd be the same"
        |> Test.toFailures
        -- Just { messages = [ "thought they'd be the same" ], context = [] }
-}
failWith : String -> Outcome -> Outcome
failWith str =
    formatFailures (\_ -> str)


{-| Apply a description to a `Test`.

-- TODO give a code example.
-}
it : String -> Outcome -> Outcome
it str test =
    case test of
        Pass ->
            Pass

        Fail record ->
            Fail { record | context = str :: record.context }


{-| A [`Test`](#Test) which failed with the given message.

-- TODO code sample
-}
fail : String -> Outcome
fail str =
    Fail { messages = [ str ], context = [] }


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
toFailures : Outcome -> Maybe { messages : List String, context : List String }
toFailures test =
    case test of
        Pass ->
            Nothing

        Fail record ->
            Just record


{-| Format all the failure messages in a given `Test`.

-- TODO code sample
-}
formatFailures : (String -> String) -> Outcome -> Outcome
formatFailures format test =
    case test of
        Fail record ->
            Fail { record | messages = List.map format record.messages }

        Pass ->
            test
