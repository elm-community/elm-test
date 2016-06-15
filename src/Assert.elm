module Assert exposing (Assertion, pass, fail, getFailure, equal, notEqual, lessThan, atMost, greaterThan, atLeast, onFail)

{-| Making assertions.

## Basic Assertions

@docs Assertion, pass, fail, getFailure, equal, notEqual

## Comparisons

function | passes if
--- | ---
[`lessThan`](#lessThan) | `secondArgument < firstArgument`
[`atMost`](#atMost) | `secondArgument <= firstArgument`
[`greaterThan`](#greaterThan) | `secondArgument > firstArgument`
[`atLeast`](#atLeast) | `secondArgument >= firstArgument`

@docs lessThan, atMost, greaterThan, atLeast

## Customizing

@docs onFail
-}

import Test.Assertion


{-| The result of a single test run: either be a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Assertion =
    Test.Assertion.Assertion


{-| Passes if the arguments are equal.

    List.length []
        |> Assert.equal 5

The failure message from this assertion would be:

    Expected 0
    to equal 5
-}
equal : a -> a -> Assertion
equal expected actual =
    if expected == actual then
        pass
    else
        fail ("Expected " ++ toString expected ++ "\nto equal " ++ toString actual)


{-| Passes if the arguments are not equal.

    (90 + 10)
        |> Assert.notEqual 100

The failure message from this assertion would be:

    Expected different values, but both were:

    100
-}
notEqual : a -> a -> Assertion
notEqual expected actual =
    if expected == actual then
        fail ("Expected different values, but both were:\n\n" ++ toString actual)
    else
        pass


{-| Passes if the second argument is less than the first.

    List.length []
        |> Assert.lessThan -1

The failure message from this assertion would be:

    Expected 0 to be less than -1.

See the [Comparisons Table](#Comparisons) for other comparisons.
-}
lessThan : comparable -> comparable -> Assertion
lessThan greater lesser =
    if lesser < greater then
        pass
    else
        fail ("Expected " ++ toString lesser ++ " to be less than " ++ toString greater)


{-| Passes if the second argument is less than or equal to the first.

    List.length []
        |> Assert.atMost -3

The failure message from this assertion would be:

    Expected 0 to be at most -3.

See the [Comparisons Table](#Comparisons) for other comparisons.
-}
atMost : comparable -> comparable -> Assertion
atMost greater lesserOrEqual =
    if lesserOrEqual <= greater then
        pass
    else
        fail ("Expected " ++ toString lesserOrEqual ++ " to be at most " ++ toString greater)


{-| Passes if the second argument is greater than the first.

    List.length []
        |> Assert.greaterThan 1

The failure message from this assertion would be:

    Expected 0 to be greater than 1.

See the [Comparisons Table](#Comparisons) for other comparisons.
-}
greaterThan : comparable -> comparable -> Assertion
greaterThan lesser greater =
    if greater > lesser then
        pass
    else
        fail ("Expected the value " ++ toString greater ++ "\nto be greater than " ++ toString lesser)


{-| Passes if the second argument is greater than or equal to the first.

    List.length []
        |> Assert.atLeast 3

The failure message from this assertion would be:

    Expected 0 to be at least 3.

See the [Comparisons Table](#Comparisons) for other comparisons.
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

    "something"
        |> Assert.equal "something else"
        |> Assert.onFail "thought those two strings would be the same"
-}
onFail : String -> Assertion -> Assertion
onFail str assertion =
    case assertion of
        Test.Assertion.Pass ->
            assertion

        Test.Assertion.Fail _ ->
            fail str
