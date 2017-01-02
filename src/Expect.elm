module Expect
    exposing
        ( Expectation
        , pass
        , fail
        , getFailure
        , equal
        , notEqual
        , atMost
        , lessThan
        , greaterThan
        , atLeast
        , within
        , notWithin
        , true
        , false
        , err
        , equalLists
        , equalDicts
        , equalSets
        , onFail
        , all
        )

{-| A library to create `Expectation`s, which describe a claim to be tested.

## Quick Reference

* [`equal`](#equal) `(arg2 == arg1)`
* [`notEqual`](#notEqual) `(arg2 /= arg1)`
* [`lessThan`](#lessThan) `(arg2 < arg1)`
* [`atMost`](#atMost) `(arg2 <= arg1)`
* [`greaterThan`](#greaterThan) `(arg2 > arg1)`
* [`atLeast`](#atLeast) `(arg2 >= arg1)`
* [`true`](#true) `(arg == True)`
* [`false`](#false) `(arg == False)`

## Basic Expectations

@docs Expectation, equal, notEqual, all

## Comparisons

@docs lessThan, atMost, greaterThan, atLeast, within, notWithin

## Booleans

@docs true, false

## Collections

@docs err, equalLists, equalDicts, equalSets

## Customizing

@docs pass, fail, onFail, getFailure
-}

import Test.Expectation
import Test.Message exposing (failureMessage)
import Dict exposing (Dict)
import Set exposing (Set)
import String


{-| The result of a single test run: either a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Expectation =
    Test.Expectation.Expectation


{-| Passes if the arguments are equal.

    Expect.equal 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because the expected value didn't split the space in "Betty Botter"
    String.split " " "Betty Botter bought some butter"
        |> Expect.equal [ "Betty Botter", "bought", "some", "butter" ]

    {-

    [ "Betty", "Botter", "bought", "some", "butter" ]
    ╷
    │ Expect.equal
    ╵
    [ "Betty Botter", "bought", "some", "butter" ]

    -}
-}
equal : a -> a -> Expectation
equal =
    equateWith "Expect.equal" (==)


{-| Passes if the arguments are not equal.

    -- Passes because (11 /= 100) is True
    90 + 10
        |> Expect.notEqual 11


    -- Fails because (100 /= 100) is False
    90 + 10
        |> Expect.notEqual 100

    {-

    100
    ╷
    │ Expect.notEqual
    ╵
    100

    -}
-}
notEqual : a -> a -> Expectation
notEqual =
    equateWith "Expect.notEqual" (/=)


{-| Passes if the second argument is less than the first.

    Expect.lessThan 1 (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.lessThan -1


    {-

    0
    ╷
    │ Expect.lessThan
    ╵
    -1

    -}
-}
lessThan : comparable -> comparable -> Expectation
lessThan =
    compareWith "Expect.lessThan" (<)


{-| Passes if the second argument is less than or equal to the first.

    Expect.atMost 1 (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.atMost -3

    {-

    0
    ╷
    │ Expect.atMost
    ╵
    -3

    -}
-}
atMost : comparable -> comparable -> Expectation
atMost =
    compareWith "Expect.atMost" (<=)


{-| Passes if the second argument is greater than the first.

    Expect.greaterThan -2 List.length []

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.greaterThan 1

    {-

    0
    ╷
    │ Expect.greaterThan
    ╵
    1

    -}
-}
greaterThan : comparable -> comparable -> Expectation
greaterThan =
    compareWith "Expect.greaterThan" (>)


{-| Passes if the second argument is greater than or equal to the first.

    Expect.atLeast -2 (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.atLeast 3

    {-

    0
    ╷
    │ Expect.atLeast
    ╵
    3

    -}
-}
atLeast : comparable -> comparable -> Expectation
atLeast =
    compareWith "Expect.atLeast" (>=)


{-| Passes if the second and third arguments are equal within a relative tolerance
specified by the first argument. This is intended to avoid failing because of
minor inaccuracies introduced by floating point arithmetic. Don't use a negative
tolerance.

    -- Fails because 0.1 + 0.2 == 0.30000000000000004 (0.1 is irrational in base 2)
    0.1 + 0.2 |> Expect.equal 0.3

    -- So instead write this test, which passes
    0.1 + 0.2 |> Expect.within 0.000000001 0.3


Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because 3.14 is not close enough to pi
    3.14 |> Expect.within 0.0001 pi

    {-

    3.14
    ╷
    │ Expect.within 0.0001
    ╵
    3.141592653589793

    -}
-}
within : Float -> Float -> Float -> Expectation
within tolerance =
    compareWith ("Expect.within " ++ toString tolerance)
        (withinCompare tolerance)


{-| Passes if (and only if) a call to `within` with the same arguments would have failed.
-}
notWithin : Float -> Float -> Float -> Expectation
notWithin tolerance =
    compareWith ("Expect.notWithin " ++ toString tolerance)
        (\a b -> not <| withinCompare tolerance a b)


withinCompare : Float -> Float -> Float -> Bool
withinCompare tolerance a b =
    let
        delta =
            abs (a - b)

        -- largest non-infinite value expressible in a 64-bit float
        float64maxValue =
            (2.0 - (2.0 ^ -52)) * 2.0 ^ 1023

        -- smallest positive value representable in a 64-bit float with a non-zero radix
        -- the smallest normal number below which we start loosing precision,
        -- so that's when we need to look at absolute differences
        float64MinNormal =
            2.0 ^ -1022
    in
        if tolerance < 0.0 then
            -- No value is that close to another value. Use a non-negative tolerance.
            -- The pragmatic way would be to use the absolute of the tolerance, but that
            -- would only help with the tests for this module itself; otherwise the tolerance
            -- argument is almost always a constant literal.
            False
        else if a == b then
            -- if they're *exactly* equal
            True
        else if a == 0 || b == 0 || delta < float64MinNormal then
            -- very close to zero; use absolute tolerance relative to smallest possible float numbers
            -- (floating point arithmetic has very large relative errors near zero)
            Debug.log (toString ( "potato", "b", a, b, delta, "tol", tolerance, (tolerance * float64MinNormal) )) <|
                (delta <= (tolerance * float64MinNormal))
        else
            -- otherwise, we use relative equality. Tolerance acts as a maximum multiplier between a and b.
            let
                -- avoid dividing by infinity; use the largest available non-inf value instead
                abSum =
                    min ((abs a + abs b) / 2.0) float64maxValue

                absmin =
                    if abs a < abs b || (abs a == abs b && a < b) then
                        a
                    else
                        b

                absmax =
                    if abs a > abs b || (abs a == abs b && a > b) then
                        a
                    else
                        b
            in
                -- (|a|+|b|)/(mean |a| |b|) is capped at [0,2], so we cannot say that the relative difference should be 3x.
                -- maybe this algorithm?:
                -- and:
                -- (absmin a b) * (1+tol) > absmax a b
                -- (absmin a b) * (1-tol) < absmax a b
                Debug.log
                    (toString ( "absmin", absmin, "absmax", absmax, "tol", tolerance ))
                <|
                    Debug.log
                        (toString ( "low", absmin, "+", (abs absmin), "*", -tolerance, "=", (absmin) + (abs absmin) * (-tolerance) ))
                    <|
                        Debug.log
                            (toString ( "high", absmin, "+", (abs absmin), "*", tolerance, "=", (absmin) + (abs absmin) * tolerance ))
                        <|
                            Debug.log
                                (toString
                                    ( "parts"
                                    , (absmin) + (abs absmin) * (-tolerance) <= absmax
                                    , (absmax <= (absmin) + (abs absmin) * (tolerance))
                                    )
                                )
                            <|
                                ((absmin) + (abs absmin) * (-tolerance) <= absmax)
                                    && (absmax <= (absmin) + (abs absmin) * (tolerance))


{-| Passes if the argument is 'True', and otherwise fails with the given message.

    Expect.true "Expected the list to be empty." (List.isEmpty [])

    -- Passes because (List.isEmpty []) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because List.isEmpty returns False, but we expect True.
    List.isEmpty [ 42 ]
        |> Expect.true "Expected the list to be empty."

    {-

    Expected the list to be empty.

    -}
-}
true : String -> Bool -> Expectation
true message bool =
    if bool then
        pass
    else
        fail message


{-| Passes if the argument is 'False', and otherwise fails with the given message.

    Expect.false "Expected the list not to be empty." (List.isEmpty [ 42 ])

    -- Passes because (List.isEmpty [ 42 ]) is False

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (List.isEmpty []) is True
    List.isEmpty []
        |> Expect.false "Expected the list not to be empty."

    {-

    Expected the list not to be empty.

    -}
-}
false : String -> Bool -> Expectation
false message bool =
    if bool then
        fail message
    else
        pass


{-| Passes if the
[`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) is
an `Err` rather than `Ok`. This is useful for tests where you expect to get an
error but you don't care about what the actual error is. If your possibly
erroring function returns a `Maybe`, simply use `Expect.equal Nothing`.

    -- Passes
    String.toInt "not an int"
        |> Expect.err

Test failures will be printed with the unexpected `Ok` value contrasting with
any `Err`.

    -- Fails
    String.toInt "20"
        |> Expect.err

    {-

    Ok 20
    ╷
    │ Expect.err
    ╵
    Err _

    -}
-}
err : Result a b -> Expectation
err result =
    case result of
        Ok _ ->
            { given = ""
            , description = "Expect.err"
            , reason = Test.Expectation.Comparison "Err _" (toString result)
            }
                |> Test.Expectation.Fail

        Err _ ->
            pass


{-| Passes if the arguments are equal lists.

    -- Passes
    [1, 2, 3]
        |> Expect.equalLists [1, 2, 3]

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which index the lists first
differed at or which list was longer:

    -- Fails
    [ 1, 2, 4, 6 ]
        |> Expect.equalLists [ 1, 2, 5 ]

    {-

    [1,2,4,6]
    first diff at index index 2: +`4`, -`5`
    ╷
    │ Expect.equalLists
    ╵
    first diff at index index 2: +`5`, -`4`
    [1,2,5]

    -}
-}
equalLists : List a -> List a -> Expectation
equalLists expected actual =
    if expected == actual then
        pass
    else
        let
            result =
                List.map2 (,) expected actual
                    |> List.indexedMap (,)
                    |> List.filterMap
                        (\( index, ( e, a ) ) ->
                            if e == a then
                                Nothing
                            else
                                Just ( index, e, a )
                        )
                    |> List.head
                    |> Maybe.map
                        (\( index, e, a ) ->
                            let
                                reason =
                                    Test.Expectation.ListDiff
                                        (toString expected)
                                        (toString actual)
                                        ( index, toString e, toString a )
                            in
                                Test.Expectation.Fail
                                    { given = ""
                                    , description = "Expect.equalLists"
                                    , reason = reason
                                    }
                        )
        in
            case result of
                Just failure ->
                    failure

                Nothing ->
                    case compare (List.length actual) (List.length expected) of
                        GT ->
                            reportFailure "Expect.equalLists was longer than" (toString expected) (toString actual)

                        LT ->
                            reportFailure "Expect.equalLists was shorter than" (toString expected) (toString actual)

                        _ ->
                            pass


{-| Passes if the arguments are equal dicts.

    -- Passes
    (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each dict:

    -- Fails
    (Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ])

    {-

    Dict.fromList [(1,"one"),(2,"too")]
    diff: -[ (2,"two"), (3,"three") ] +[ (2,"too") ]
    ╷
    │ Expect.equalDicts
    ╵
    diff: +[ (2,"two"), (3,"three") ] -[ (2,"too") ]
    Dict.fromList [(1,"one"),(2,"two"),(3,"three")]

    -}
-}
equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts expected actual =
    if Dict.toList expected == Dict.toList actual then
        pass
    else
        let
            differ dict k v diffs =
                if Dict.get k dict == Just v then
                    diffs
                else
                    ( k, v ) :: diffs

            missingKeys =
                Dict.foldr (differ actual) [] expected

            extraKeys =
                Dict.foldr (differ expected) [] actual
        in
            reportCollectionFailure "Expect.equalDicts" expected actual missingKeys extraKeys


{-| Passes if the arguments are equal sets.

    -- Passes
    (Set.fromList [1, 2])
        |> Expect.equalSets (Set.fromList [1, 2])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each set:

    -- Fails
    (Set.fromList [ 1, 2, 4, 6 ])
        |> Expect.equalSets (Set.fromList [ 1, 2, 5 ])

    {-

    Set.fromList [1,2,4,6]
    diff: -[ 5 ] +[ 4, 6 ]
    ╷
    │ Expect.equalSets
    ╵
    diff: +[ 5 ] -[ 4, 6 ]
    Set.fromList [1,2,5]

    -}
-}
equalSets : Set comparable -> Set comparable -> Expectation
equalSets expected actual =
    if Set.toList expected == Set.toList actual then
        pass
    else
        let
            missingKeys =
                Set.diff expected actual
                    |> Set.toList

            extraKeys =
                Set.diff actual expected
                    |> Set.toList
        in
            reportCollectionFailure "Expect.equalSets" expected actual missingKeys extraKeys


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
fail str =
    Test.Expectation.Fail { given = "", description = str, reason = Test.Expectation.Custom }


{-| Return `Nothing` if the given [`Expectation`](#Expectation) is a [`pass`](#pass).

If it is a [`fail`](#fail), return a record containing the failure message,
along with the given inputs if it was a fuzz test. (If no inputs were involved,
the record's `given` field will be `""`).

For example, if a fuzz test generates random integers, this might return
`{ message = "it was supposed to be positive", given = "-1" }`

    getFailure (Expect.fail "this failed")
    -- Just { message = "this failed", given = "" }

    getFailure (Expect.pass)
    -- Nothing
-}
getFailure : Expectation -> Maybe { given : String, message : String }
getFailure expectation =
    case expectation of
        Test.Expectation.Pass ->
            Nothing

        Test.Expectation.Fail record ->
            Just { given = record.given, message = failureMessage record }


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.equal "something else"
        |> Expect.onFail "thought those two strings would be the same"
-}
onFail : String -> Expectation -> Expectation
onFail str expectation =
    case expectation of
        Test.Expectation.Pass ->
            expectation

        Test.Expectation.Fail failure ->
            Test.Expectation.Fail { failure | description = str, reason = Test.Expectation.Custom }


{-| Passes if each of the given functions passes when applied to the subject.
**NOTE:** Passing an empty list is assumed to be a mistake, so `Expect.all []`
will always return a failed expectation no matter what else it is passed.
    Expect.all
        [ Expect.greaterThan -2
        , Expect.lessThan 5
        ]
        (List.length [])
    -- Passes because (0 > -2) is True and (0 < 5) is also True
Failures resemble code written in pipeline style, so you can tell
which argument is which:
    -- Fails because (0 > -10) is False
    List.length []
        |> Expect.all
            [ Expect.greaterThan -2
            , Expect.lessThan -10
            , Expect.equal 0
            ]
    {-
    0
    ╷
    │ Expect.lessThan
    ╵
    -10
    -}
-}
all : List (subject -> Expectation) -> subject -> Expectation
all list query =
    if List.isEmpty list then
        fail "Expect.all received an empty list. I assume this was due to a mistake somewhere, so I'm failing this test!"
    else
        allHelp list query


allHelp : List (subject -> Expectation) -> subject -> Expectation
allHelp list query =
    case list of
        [] ->
            pass

        check :: rest ->
            case check query of
                Test.Expectation.Pass ->
                    allHelp rest query

                outcome ->
                    outcome



{---- Private helper functions ----}


reportFailure : String -> String -> String -> Expectation
reportFailure comparison expected actual =
    { given = ""
    , description = comparison
    , reason = Test.Expectation.Comparison (toString expected) (toString actual)
    }
        |> Test.Expectation.Fail


reportCollectionFailure : String -> a -> b -> List c -> List d -> Expectation
reportCollectionFailure comparison expected actual missingKeys extraKeys =
    { given = ""
    , description = comparison
    , reason =
        { expected = toString expected
        , actual = toString actual
        , extra = List.map toString extraKeys
        , missing = List.map toString missingKeys
        }
            |> Test.Expectation.CollectionDiff
    }
        |> Test.Expectation.Fail


{-| String arg is label, e.g. "Expect.equal".
-}
equateWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
equateWith =
    testWith Test.Expectation.Equals


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith =
    testWith Test.Expectation.Comparison


testWith : (String -> String -> Test.Expectation.Reason) -> String -> (a -> b -> Bool) -> b -> a -> Expectation
testWith makeReason label runTest expected actual =
    if runTest actual expected then
        pass
    else
        { given = ""
        , description = label
        , reason = makeReason (toString expected) (toString actual)
        }
            |> Test.Expectation.Fail
