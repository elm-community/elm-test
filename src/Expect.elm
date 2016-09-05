module Expect exposing (Expectation, pass, fail, getFailure, equal, notEqual, atMost, lessThan, greaterThan, atLeast, true, false, equalLists, equalDicts, equalSets, onFail)

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

@docs Expectation, equal, notEqual

## Comparisons

@docs lessThan, atMost, greaterThan, atLeast

## Booleans

@docs true, false

## Collections

@docs equalLists, equalDicts, equalSets

## Customizing

@docs pass, fail, onFail, getFailure
-}

import Test.Expectation
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
    compareWith "Expect.equal" (==)


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
    compareWith "Expect.notEqual" (/=)


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


{-| Passes if the arguments are equal lists.

    -- Passes
    [1, 2, 3]
        |> Expect.equalLists [1, 2, 3]

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which index the lists first
differed at:

    -- Fails
    [ 1, 2, 4, 6 ]
        |> Expect.equalLists [ 1, 2, 5 ]

    {-

    [1,2,4,6]
    ╷
    │ Expect.equalLists: differed at index 2. Expected `4`, got `5`.
    ╵
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
                List.map2 (,) actual expected
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
                            fail <|
                                reportFailure
                                    ("Expect.equalLists: differed at index "
                                        ++ toString index
                                        ++ ". Expected `"
                                        ++ toString e
                                        ++ "`, got `"
                                        ++ toString a
                                        ++ "`."
                                    )
                                    (toString expected)
                                    (toString actual)
                        )
        in
            case result of
                Just failure ->
                    failure

                Nothing ->
                    case compare (List.length actual) (List.length expected) of
                        GT ->
                            reportFailure "Expect.equalLists: was longer than expected" (toString expected) (toString actual)
                                |> fail

                        LT ->
                            reportFailure "Expect.equalLists: was shorter than expected" (toString expected) (toString actual)
                                |> fail

                        _ ->
                            pass


{-| Passes if the arguments are equal dicts.

    -- Passes
    (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from or added to the actual dict and which had different values:

    -- Fails
    (Dict.fromList [ ( 1, "one" ), ( 2, "too" ), ( 5, "five" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ), ( 4, "four" ) ])

    {-

    Dict.fromList [(1,"one"),(2,"too"),(5,"five")]
    ╷
    │ Expect.equalDicts
    ╵
    Dict.fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
    Was missing keys: -[ 3, 4 ]
    Had extra keys: +[ 5 ]
    Diffs:
      For key: 2
        Expected: "two"
        Actual: "too"

    -}
-}
equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts expected actual =
    if Dict.toList expected == Dict.toList actual then
        pass
    else
        let
            differ k v ( missingKeys, diffs ) =
                if not <| Dict.member k actual then
                    ( Set.insert k missingKeys, diffs )
                else
                    let
                        actualVal =
                            Dict.get k actual
                    in
                        if actualVal == Just v then
                            ( missingKeys, diffs )
                        else
                            ( missingKeys, ( k, v, actualVal ) :: diffs )

            ( missingKeys, diffs ) =
                Dict.foldr differ ( Set.empty, [] ) expected

            diffsMessage =
                diffs
                    |> List.map
                        (\( key, expectedVal, actualVal ) ->
                            [ "  For key: " ++ toString key
                            , "    Expected: " ++ toString expectedVal
                            , "    Actual: " ++ (actualVal |> Maybe.map toString |> Maybe.withDefault "")
                            ]
                        )
                    |> List.concat
                    |> String.join "\n"

            extraKeys =
                Set.diff (Set.fromList <| Dict.keys actual) (Set.fromList <| Dict.keys expected)
                    |> formatSet Extra

            baseFailureMessage =
                (reportFailure "Expect.equalDicts" (toString expected) (toString actual))

            failureMessage =
                [ baseFailureMessage
                , "Was missing keys: " ++ formatSet Missing missingKeys
                , "Had extra keys: " ++ extraKeys
                , "Diffs:"
                , diffsMessage
                ]
                    |> String.join "\n"
        in
            fail failureMessage


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

            extraKeys =
                Set.diff actual expected

            failureMessage =
                [ toString actual
                , "diff:" ++ formatSet Missing missingKeys ++ formatSet Extra extraKeys
                , "╷"
                , "│ Expect.equalSets"
                , "╵"
                , "diff:" ++ formatSet Extra missingKeys ++ formatSet Missing extraKeys
                , toString expected
                ]
                    |> String.join "\n"
        in
            fail failureMessage


type Diff
    = Extra
    | Missing


formatSet : Diff -> Set comparable -> String
formatSet diff set =
    if Set.isEmpty set then
        ""
    else
        let
            modifier =
                case diff of
                    Extra ->
                        "+"

                    Missing ->
                        "-"
        in
            set
                |> Set.toList
                |> List.map toString
                |> String.join ", "
                |> (\s -> " " ++ modifier ++ "[ " ++ s ++ " ]")


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
    Test.Expectation.Fail ""


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

        Test.Expectation.Fail given message ->
            Just { given = given, message = message }


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

        Test.Expectation.Fail given _ ->
            Test.Expectation.Fail given str


reportFailure : String -> String -> String -> String
reportFailure comparison expected actual =
    [ actual
    , "╷"
    , "│ " ++ comparison
    , "╵"
    , expected
    ]
        |> String.join "\n"


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith label compare expected actual =
    if compare actual expected then
        pass
    else
        fail (reportFailure label (toString expected) (toString actual))
