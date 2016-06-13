module Assert exposing (Assertion, pass, fail, getFailure, equal, notEqual, lessThan, greaterThan, onFail)

{-| Making assertions.


## Basic Assertions

@docs Assertion, pass, fail, getFailure

## Comparisons

@docs equal, notEqual, lessThan, greaterThan

## Customizing

@docs onFail
-}

import Test.Assertion


{-| The result of a single test run: either be a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Assertion =
    Test.Assertion.Assertion


{-| Fails if `expected /= actual`.
-}
equal : { expected : a, actual : a } -> Assertion
equal { expected, actual } =
    if expected == actual then
        pass
    else
        fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


{-| Fails if `actual == wasNot`.
-}
notEqual : { actual : a, wasNot : a } -> Assertion
notEqual record =
    if record.actual == record.wasNot then
        fail ("Expected different values, but both were:\n\n" ++ toString record.actual)
    else
        pass


{-| Fails if `lesser >= greater`.

(This function is identical to [`greaterThan`](#greaterThan).)
-}
lessThan : { lesser : comparable, greater : comparable } -> Assertion
lessThan =
    greaterThan


{-| Fails if `lesser >= greater`.

(This function is identical to [`lessThan`](#lessThan).)
-}
greaterThan : { lesser : comparable, greater : comparable } -> Assertion
greaterThan { lesser, greater } =
    if lesser < greater then
        pass
    else
        fail ("Expected Greater: " ++ toString greater ++ "\nExpected Lesser:  " ++ toString lesser)


{-| Always passes.

-- TODO code sample
-}
pass : Assertion
pass =
    Test.Assertion.Pass


{-| Fails with the given message.

-- TODO code sample
-}
fail : String -> Assertion
fail =
    Test.Assertion.Fail


{-| Fails with the given message.

-- TODO code sample
-}
getFailure : Assertion -> Maybe String
getFailure assertion =
    case assertion of
        Test.Assertion.Pass ->
            Nothing

        Test.Assertion.Fail desc ->
            Just desc


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (onFail)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> Assert.onFail "thought they'd be the same"
-}
onFail : String -> Assertion -> Assertion
onFail str assertion =
    case assertion of
        Test.Assertion.Pass ->
            assertion

        Test.Assertion.Fail _ ->
            fail str



-- TODO should add something like equalLists, equalDicts, and equalSets, which
-- output useful diffs on failure.
