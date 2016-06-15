module Assert exposing (Assertion, pass, fail, getFailure, equal, notEqual, lessThan, atMost, greaterThan, atLeast, onFail)

{-| Making assertions.


## Basic Assertions

@docs Assertion, pass, fail, getFailure

## Comparisons

@docs equal, notEqual, lessThan, atMost, greaterThan, atLeast

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


{-| Passes if `secondArgument < firstArgument`

    -- This assertion will PASS.
    Assert.lessThan 1 (List.length [])

function | passes if
--- | ---
[`lessThan`](#lessThan) | `secondArgument < firstArgument`
[`atMost`](#atMost) | `secondArgument <= firstArgument`
[`greaterThan`](#greaterThan) | `secondArgument > firstArgument`
[`atLeast`](#atLeast) | `secondArgument >= firstArgument`
-}
lessThan : comparable -> comparable -> Assertion
lessThan greater lesser =
    if lesser < greater then
        pass
    else
        fail ("Expected " ++ toString lesser ++ " to be less than " ++ toString greater)


{-| Passes if `secondArgument <= firstArgument`

    -- This assertion will PASS.
    Assert.atMost 0 (List.length [])

function | passes if
--- | ---
[`lessThan`](#lessThan) | `secondArgument < firstArgument`
[`atMost`](#atMost) | `secondArgument <= firstArgument`
[`greaterThan`](#greaterThan) | `secondArgument > firstArgument`
[`atLeast`](#atLeast) | `secondArgument >= firstArgument`
-}
atMost : comparable -> comparable -> Assertion
atMost greater lesserOrEqual =
    if lesserOrEqual <= greater then
        pass
    else
        fail ("Expected " ++ toString lesserOrEqual ++ " to be at most " ++ toString greater)


{-| Passes if `secondArgument > firstArgument`

    -- This assertion will FAIL.
    Assert.greaterThan 0 (List.length [])

function | passes if
--- | ---
[`lessThan`](#lessThan) | `secondArgument < firstArgument`
[`atMost`](#atMost) | `secondArgument <= firstArgument`
[`greaterThan`](#greaterThan) | `secondArgument > firstArgument`
[`atLeast`](#atLeast) | `secondArgument >= firstArgument`
-}
greaterThan : comparable -> comparable -> Assertion
greaterThan lesser greater =
    if greater > lesser then
        pass
    else
        fail ("Expected the value " ++ toString greater ++ "\nto be greater than " ++ toString lesser)


{-| Passes if `secondArgument >= firstArgument`

    -- This assertion will PASS.
    Assert.atLeast 0 (List.length [])

function | passes if
--- | ---
[`lessThan`](#lessThan) | `secondArgument < firstArgument`
[`atMost`](#atMost) | `secondArgument <= firstArgument`
[`greaterThan`](#greaterThan) | `secondArgument > firstArgument`
[`atLeast`](#atLeast) | `secondArgument >= firstArgument`
-}
atLeast : comparable -> comparable -> Assertion
atLeast greaterOrEqual lesser =
    if lesser >= greaterOrEqual then
        pass
    else
        fail ("Expected " ++ toString lesser ++ " to be at least " ++ toString greaterOrEqual)


{-| Always passes.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Assert


    test "Jon.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Assert.pass

                Err err ->
                    Assert.fail err
-}
pass : Assertion
pass =
    Test.Assertion.Pass


{-| Fails with the given message.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Assert


    test "Jon.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Assert.pass

                Err err ->
                    Assert.fail err
-}
fail : String -> Assertion
fail =
    Test.Assertion.Fail


{-| Return `Nothing` if the given [`Assertion`](#Assertion) is a [`pass`](#pass),
and `Just` the error message if it is a [`fail`](#fail).

    getFailure (Assert.fail "this assertion failed")
    -- Just "this assertion failed"

    getFailure (Assert.pass)
    -- Nothing
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
