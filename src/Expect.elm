module Expect exposing (Expectation, pass, fail, getFailure, toEqual, notToEqual, toBeAtMost, toBeLessThan, toBeGreaterThan, toBeAtLeast, toBeTrue, toBeFalse, onFail, all)

{-| Determining whether tests pass or fail.

## Quick Reference

* [`toEqual`](#toEqual) `(arg2 == arg1)`
* [`notToEqual`](#notToEqual) `(arg2 /= arg1)`
* [`toBeLessThan`](#toBeLessThan) `(arg2 < arg1)`
* [`toBeAtMost`](#toBeAtMost) `(arg2 <= arg1)`
* [`toBeGreaterThan`](#toBeGreaterThan) `(arg2 > arg1)`
* [`toBeAtLeast`](#toBeAtLeast) `(arg2 >= arg1)`
* [`toBeTrue`](#toBeTrue) `(arg == True)`
* [`toBeFalse`](#toBeFalse) `(arg == False)`

## Basic Expectations

@docs Expectation, toEqual, notToEqual, all

## Comparisons

@docs toBeLessThan, toBeAtMost, toBeGreaterThan, toBeAtLeast

## Booleans

@docs toBeTrue, toBeFalse

## Customizing

@docs pass, fail, onFail, getFailure
-}

import Test.Expectation
import String


{-| The result of a single test run: either a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Expectation =
    Test.Expectation.Expectation


{-| Passes if the arguments are equal.

    Expect.toEqual 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 == 5) is False
    List.length []
        |> Expect.toEqual 5


    {-

    Expected  0

    toEqual   5

    -}
-}
toEqual : a -> a -> Expectation
toEqual expected actual =
    if expected == actual then
        pass
    else
        reportFailure "toEqual" (toString expected) (toString actual)
            |> fail


{-| Passes if the arguments are not equal.

    Expect.notToEqual 11 (90 + 10)

    -- Passes because (11 /= 100) is True

Failures only show one value, because the reason for the failure was that
both arguments were equal.

    -- Fails because (100 /= 100) is False
    (90 + 10)
        |> Expect.notToEqual 100

    {-

    Expected different values, but both were equal to:

    100

    -}
-}
notToEqual : a -> a -> Expectation
notToEqual expected actual =
    if actual == expected then
        [ "Expected different values, but both were equal to:"
        , toString actual
        ]
            |> String.join "\n\n"
            |> fail
    else
        pass


{-| Passes if the second argument is less than the first.

    Expect.toBeLessThan 1 (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.toBeLessThan -1


    {-

    Expected      0

    toBeLessThan  -1

    -}
-}
toBeLessThan : comparable -> comparable -> Expectation
toBeLessThan expected actual =
    if actual < expected then
        pass
    else
        reportFailure "toBeLessThan" (toString expected) (toString actual)
            |> fail


{-| Passes if the second argument is less than or equal to the first.

    Expect.toBeAtMost 1 (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.toBeAtMost -3

    {-

    Expected    0

    toBeAtMost  -3

    -}
-}
toBeAtMost : comparable -> comparable -> Expectation
toBeAtMost expected actual =
    if actual <= expected then
        pass
    else
        reportFailure "toBeAtMost" (toString expected) (toString actual)
            |> fail


{-| Passes if the second argument is greater than the first.

    Expect.toBeGreaterThan -2 List.length []

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.toBeGreaterThan 1

    {-

    Expected         0

    toBeGreaterThan  1

    -}
-}
toBeGreaterThan : comparable -> comparable -> Expectation
toBeGreaterThan expected actual =
    if actual > expected then
        pass
    else
        reportFailure "toBeGreaterThan" (toString expected) (toString actual)
            |> fail


{-| Passes if the second argument is greater than or equal to the first.

    Expect.toBeAtLeast -2 (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.toBeAtLeast 3

    {-

    Expected     0

    toBeAtLeast  3

    -}
-}
toBeAtLeast : comparable -> comparable -> Expectation
toBeAtLeast expected actual =
    if actual >= expected then
        pass
    else
        reportFailure "toBeAtLeast" (toString expected) (toString actual)
            |> fail


{-| Passes if the argument is 'True', and otherwise fails with the given message.

    Expect.toBeTrue "Expected the list to be empty." (List.isEmpty [])

    -- Passes because (List.isEmpty []) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because List.isEmpty returns False, but we expect True.
    List.isEmpty [ 42 ]
        |> Expect.toBeTrue "Expected the list to be empty."

    {-

    Expected the list to be empty.

    -}
-}
toBeTrue : String -> Bool -> Expectation
toBeTrue message bool =
    if bool then
        pass
    else
        fail message


{-| Passes if the argument is 'False', and otherwise fails with the given message.

    Expect.toBeFalse "Expected the list not to be empty." (List.isEmpty [ 42 ])

    -- Passes because (List.isEmpty [ 42 ]) is False

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (List.isEmpty []) is True
    List.isEmpty []
        |> Expect.toBeFalse "Expected the list not to be empty."

    {-

    Expected the list not to be empty.

    -}
-}
toBeFalse : String -> Bool -> Expectation
toBeFalse message bool =
    if bool then
        fail message
    else
        pass


{-| Always passes.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \() ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err
-}
pass : Expectation
pass =
    Test.Expectation.Pass


{-| Fails with the given message.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \() ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err
-}
fail : String -> Expectation
fail =
    Test.Expectation.Fail


{-| Return `Nothing` if the given [`Expectation`](#Expectation) is a [`pass`](#pass),
and `Just` the error message if it is a [`fail`](#fail).

    getFailure (Expect.fail "this failed")
    -- Just "this failed"

    getFailure (Expect.pass)
    -- Nothing
-}
getFailure : Expectation -> Maybe String
getFailure expectation =
    case expectation of
        Test.Expectation.Pass ->
            Nothing

        Test.Expectation.Fail desc ->
            Just desc


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.toEqual "something else"
        |> Expect.onFail "thought those two strings would be the same"
-}
onFail : String -> Expectation -> Expectation
onFail str expectation =
    case expectation of
        Test.Expectation.Pass ->
            expectation

        Test.Expectation.Fail _ ->
            fail str


{-| Translate each element in a list into an [`Expectation`](#Expectation). If
they all pass, return a pass. If any fail, return a fail whose message includes
all the other failure messages.

    [ 0, 1, 2, 3, 4, 5 ]
        |> Expect.all (Expect.toBeLessThan 3)

    {-

    Expected      3

    toBeLessThan  3

    ---

    Expected      4

    toBeLessThan  3

    ---

    Expected      5

    toBeLessThan  3

    -}
-}
all : (a -> Expectation) -> List a -> Expectation
all getExpectation list =
    case List.filterMap (getExpectation >> getFailure) list of
        [] ->
            pass

        failures ->
            failures
                |> String.join "\n\n---\n\n"
                |> fail


reportFailure : String -> String -> String -> String
reportFailure actualCaption expected actual =
    if String.length expected < compactModeLength && String.length actual < compactModeLength then
        let
            -- Leave two spaces for a distinctive margin.
            padTo =
                2 + max (String.length expected) (String.length actualCaption)

            pad =
                String.padRight padTo ' '
        in
            [ pad expectedCaption ++ expected
            , pad actualCaption ++ actual
            ]
                |> String.join "\n\n"
    else
        [ withUnderline expectedCaption
        , expected
        , ""
        , withUnderline actualCaption
        , actual
        ]
            |> String.join "\n"


expectedCaption : String
expectedCaption =
    "Expected"


withUnderline : String -> String
withUnderline str =
    str ++ "\n" ++ String.repeat (String.length str) "-"


compactModeLength : Int
compactModeLength =
    64
