module Assert exposing (Assertion, pass, fail, getFailure, equal, notEqual, lessThan, atMost, greaterThan, atLeast, true, false, onFail, all)

{-| Determining whether tests pass or fail.

## Quick Reference

* [`equal`](#equal) `(arg2 == arg1)`
* [`notEqual`](#notEqual) `(arg2 /= arg1)`
* [`lessThan`](#lessThan) `(arg2 < arg1)`
* [`atMost`](#atMost) `(arg2 <= arg1)`
* [`greaterThan`](#greaterThan) `(arg2 > arg1)`
* [`atLeast`](#atLeast) `(arg2 >= arg1)`
* [`true`](#true) `(arg == True)`
* [`false`](#false) `(arg == False)`

## Basic Assertions

@docs Assertion, equal, notEqual, all

## Comparisons

@docs lessThan, atMost, greaterThan, atLeast

## Booleans

@docs true, false

## Customizing

@docs pass, fail, onFail, getFailure
-}

import Test.Assertion
import String


{-| The result of a single test run: either a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Assertion =
    Test.Assertion.Assertion


{-| Passes if the arguments are equal.

    Assert.equal 5 (List.length [])

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (0 == 5) is False
    List.length []
        |> Assert.equal 5


    {-

    We expected arg2 == arg1, but got:

    Assert.equal

    5

    0

    -}
-}
equal : a -> a -> Assertion
equal expected actual =
    if actual == expected then
        pass
    else
        [ "We expected arg2 == arg1, but got:"
        , "Assert.equal"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail


{-| Passes if the arguments are not equal.

    Assert.notEqual 100 (90 + 10)

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (100 /= 100) is False
    (90 + 10)
        |> Assert.notEqual 100

    {-

    We expected arg2 /= arg1, but got:

    Assert.notEqual

    100

    100

    -}
-}
notEqual : a -> a -> Assertion
notEqual expected actual =
    if actual == expected then
        [ "We expected arg2 /= arg1, but got:"
        , "Assert.notEqual"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail
    else
        pass


{-| Passes if the second argument is less than the first.

    Assert.lessThan -1 (List.length [])

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (0 < -1) is False
    List.length []
        |> Assert.lessThan -1


    {-

    We expected arg2 < arg1, but got:

    Assert.lessThan

    -1

    0

    -}
-}
lessThan : comparable -> comparable -> Assertion
lessThan expected actual =
    if actual < expected then
        pass
    else
        [ "We expected arg2 < arg1, but got:"
        , "Assert.lessThan"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail


{-| Passes if the second argument is less than or equal to the first.

    Assert.atMost -3 (List.length [])

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (0 <= -3) is False
    List.length []
        |> Assert.atMost -3

    {-

    We expected arg2 <= arg1, but got:

    Assert.atMost

    -3

    0

    -}
-}
atMost : comparable -> comparable -> Assertion
atMost expected actual =
    if actual <= expected then
        pass
    else
        [ "We expected arg2 <= arg1, but got:"
        , "Assert.atMost"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail


{-| Passes if the second argument is greater than the first.

    Assert.greaterThan 1 List.length []

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (0 > 1) is False
    List.length []
        |> Assert.greaterThan 1

    {-

    We expected arg2 > arg1, but got:

    Assert.greaterThan

    1

    0

    -}
-}
greaterThan : comparable -> comparable -> Assertion
greaterThan expected actual =
    if actual > expected then
        pass
    else
        [ "We expected arg2 > arg1, but got:"
        , "Assert.greaterThan"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail


{-| Passes if the second argument is greater than or equal to the first.

    Assert.atLeast 3 (List.length [])

The first argument is the expected value. This makes things nice when writing
in pipeline style.

    -- Fails because (0 >= 3) is False
    List.length []
        |> Assert.atLeast 3

    {-

    We expected arg2 >= arg1, but got:

    Assert.atLeast

    3

    0

    -}
-}
atLeast : comparable -> comparable -> Assertion
atLeast expected actual =
    if actual >= expected then
        pass
    else
        [ "We expected arg2 >= arg1, but got:"
        , "Assert.atLeast"
        , toString expected
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail


{-| Passes if the argument is 'True', and otherwise fails with the given message.

    Assert.true "We expected the list to be empty." (List.isEmpty [ 42 ])

The first argument is the message, to make things nice when writing in
pipeline style.

    -- Fails because List.isEmpty returns False, but we expect True.
    List.isEmpty [ 42 ]
        |> Assert.true "We expected the list to be empty."

    {-

    We expected the list to be empty.

    -}
-}
true : String -> Bool -> Assertion
true message bool =
    if bool then
        pass
    else
        fail message


{-| Passes if the argument is 'False', and otherwise fails with the given message.

    Assert.false "We expected the list to have things in it." (List.isEmpty [])

The first argument is the message, to make things nice when writing in
pipeline style.

    -- Fails because List.isEmpty returns True, but we expect False.
    List.isEmpty []
        |> Assert.false "We expected the list to have things in it."

    {-

    We expected the list to have things in it.

    -}
-}
false : String -> Bool -> Assertion
false message bool =
    if bool then
        fail message
    else
        pass


{-| Always passes.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Assert


    test "Json.Decode.int can decode the number 42." <|
        \() ->
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


    test "Json.Decode.int can decode the number 42." <|
        \() ->
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


{-| If the given assertion fails, replace its failure message with a custom one.

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


{-| Translate each element in a list into an [`Assertion`](#Assertion). If
they all pass, return a pass. If any fail, return a fail whose message includes
all the other failure messages.

    [ 0, 1, 2, 3, 4, 5 ]
        |> Assert.all (Assert.lessThan 3)

    {-

    Expected 3 to be less than 3

    Expected 4 to be less than 3

    Expected 5 to be less than 3

    -}
-}
all : (a -> Assertion) -> List a -> Assertion
all getAssertion list =
    case List.filterMap (getAssertion >> getFailure) list of
        [] ->
            pass

        failures ->
            failures
                |> String.join "\n\n"
                |> fail
