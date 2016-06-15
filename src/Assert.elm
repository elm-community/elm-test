module Assert exposing (Assertion, pass, fail, getFailure, equal, notEqual, lessThan, atMost, greaterThan, atLeast, onFail, all)

{-| Determining whether tests pass or fail.

## Quick Reference

* [`equal`](#equal) `(arg2 == arg1)`
* [`notEqual`](#notEqual) `(arg2 /= arg1)`
* [`lessThan`](#lessThan) `(arg2 < arg1)`
* [`atMost`](#atMost) `(arg2 <= arg1)`
* [`greaterThan`](#greaterThan) `(arg2 > arg1)`
* [`atLeast`](#atLeast) `(arg2 >= arg1)`

## Basic Assertions

@docs Assertion, equal, notEqual, all

## Comparisons

@docs lessThan, atMost, greaterThan, atLeast

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

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (0 == 5) is False
    List.length []
        |> Assert.equal 5


    {-

    Expected 0
    to equal 5

    -}
-}
equal : a -> a -> Assertion
equal expected actual =
    if expected == actual then
        pass
    else
        fail ("Expected " ++ toString expected ++ "\nto equal " ++ toString actual)


{-| Passes if the arguments are not equal.

    Assert.notEqual 100 (90 + 10)

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (100 /= 100) is False
    (90 + 10)
        |> Assert.notEqual 100


    {-

    Expected different values, but both were:

    100

    -}
-}
notEqual : a -> a -> Assertion
notEqual expected actual =
    if expected == actual then
        fail ("Expected different values, but both were:\n\n" ++ toString actual)
    else
        pass


{-| Passes if the second argument is less than the first.

    Assert.lessThan -1 (List.length [])

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (0 < -1) is False
    List.length []
        |> Assert.lessThan -1


    {-

    Expected 0 to be less than -1.

    -}
-}
lessThan : comparable -> comparable -> Assertion
lessThan greater lesser =
    if lesser < greater then
        pass
    else
        fail ("Expected " ++ toString lesser ++ " to be less than " ++ toString greater)


{-| Passes if the second argument is less than or equal to the first.

    Assert.atMost -3 (List.length [])

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Assert.atMost -3

    {-

    Expected 0 to be at most -3.

    -}
-}
atMost : comparable -> comparable -> Assertion
atMost greater lesserOrEqual =
    if lesserOrEqual <= greater then
        pass
    else
        fail ("Expected " ++ toString lesserOrEqual ++ " to be at most " ++ toString greater)


{-| Passes if the second argument is greater than the first.

    Assert.greaterThan 1 List.length []

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (0 > 1) is False
    List.length []
        |> Assert.greaterThan 1

    {-

    Expected 0 to be greater than 1.

    -}
-}
greaterThan : comparable -> comparable -> Assertion
greaterThan lesser greater =
    if greater > lesser then
        pass
    else
        fail ("Expected the value " ++ toString greater ++ "\nto be greater than " ++ toString lesser)


{-| Passes if the second argument is greater than or equal to the first.

    Assert.atLeast 3 (List.length [])

Failure messages line up nicely with assertions written in pipeline style:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Assert.atLeast 3

    {-

    Expected 0 to be at least 3.

    -}
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
