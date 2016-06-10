module Test exposing (Test, failWith, toFailures, formatFailures, it, pass, fail)

{-| Testing

@docs Test, pass, fail, it, failWith, toFailures, formatFailures
-}


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (failWith)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> failWith "thought they'd be the same"
        |> Test.toFailures
        -- Just { messages = [ "thought they'd be the same" ], context = [] }
-}
failWith : String -> Test -> Test
failWith str =
    formatFailures (\_ -> str)


{-| Apply a description to a `Test`.

-- TODO give a code example.
-}
it : String -> Test -> Test
it str test =
    case test of
        Pass ->
            Pass

        Fail record ->
            Fail { record | context = str :: record.context }


{-| A single test run. This can either be a [`pass`](#pass) or [`fail`](#fail).

Use [`toFailures`](#toFailures) to convert a `Test` into appropriately
contextualized failure messages.
-}
type Test
    = Pass
    | Fail { messages : List String, context : List String }


{-| A [`Test`](#Test) which failed with the given message.

-- TODO code sample
-}
fail : String -> Test
fail str =
    Fail { messages = [ str ], context = [] }


{-| A [`Test`](#Test) which passed.

-- TODO code sample
-}
pass : Test
pass =
    Pass


{-| Return contextualized failure messages from the given [`Test`](#Test).

Note that fuzz tests may return multiple failure messages from a single `Test`!

-- TODO code sample
-}
toFailures : Test -> Maybe { messages : List String, context : List String }
toFailures test =
    case test of
        Pass ->
            Nothing

        Fail record ->
            Just record


{-| Format all the failure messages in a given `Test`.

-- TODO code sample
-}
formatFailures : (String -> String) -> Test -> Test
formatFailures format test =
    case test of
        Fail record ->
            Fail { record | messages = List.map format record.messages }

        Pass ->
            test
