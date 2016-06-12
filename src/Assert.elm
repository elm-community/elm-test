module Assert exposing (success, failure, equal, notEqual, lessThan, greaterThan, failWith)

{-| Functions that call `Test.pass` and `Test.fail` with helpful output when
things fail.

@docs success, failure, equal, notEqual, lessThan, greaterThan, failWith
-}

import Test.Outcome exposing (Outcome, pass, fail)


{-| Fails if `expected /= actual`.
-}
equal : { expected : a, actual : a } -> Outcome
equal { expected, actual } =
    if expected == actual then
        pass
    else
        fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


{-| Fails if `actual == wasNot`.
-}
notEqual : { actual : a, wasNot : a } -> Outcome
notEqual record =
    if record.actual == record.wasNot then
        fail ("Expected different values, but both were:\n\n" ++ toString record.actual)
    else
        pass


{-| Fails if `lesser >= greater`.

(This function is identical to [`greaterThan`](#greaterThan).)
-}
lessThan : { lesser : comparable, greater : comparable } -> Outcome
lessThan =
    greaterThan


{-| Fails if `lesser >= greater`.

(This function is identical to [`lessThan`](#lessThan).)
-}
greaterThan : { lesser : comparable, greater : comparable } -> Outcome
greaterThan { lesser, greater } =
    if lesser < greater then
        pass
    else
        fail ("Expected Greater: " ++ toString greater ++ "\nExpected Lesser:  " ++ toString lesser)


{-| Always passes.

-- TODO code sample
-}
success : Outcome
success =
    pass


{-| Fails with the given message.

-- TODO code sample
-}
failure : String -> Outcome
failure =
    fail


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (failWith)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> failWith "thought they'd be the same"
        |> Test.toFailures
        -- Just { messages = [ "thought they'd be the same" ], context = [] }
-}
failWith : String -> Outcome -> Outcome
failWith str outcome =
    if outcome == pass then
        pass
    else
        fail str



-- TODO should add something like equalLists, equalDicts, and equalSets, which
-- output useful diffs on failure.
