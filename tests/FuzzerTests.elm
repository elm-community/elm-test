module FuzzerTests exposing (fuzzerTests)

import Expect
import Fuzz exposing (..)
import Helpers exposing (..)
import Lazy.List
import Random.Pcg as Random
import RoseTree
import Test exposing (..)
import Test.Runner


die : Fuzzer Int
die =
    Fuzz.intRange 1 6


seed : Fuzzer Random.Seed
seed =
    Fuzz.custom
        (Random.int Random.minInt Random.maxInt |> Random.map Random.initialSeed)
        (always Lazy.List.empty)


fuzzerTests : Test
fuzzerTests =
    describe "Fuzzer methods that use Debug.crash don't call it"
        [ describe "FuzzN (uses tupleN) testing string length properties"
            [ fuzz2 string string "fuzz2" <|
                \a b ->
                    testStringLengthIsPreserved [ a, b ]
            , fuzz3 string string string "fuzz3" <|
                \a b c ->
                    testStringLengthIsPreserved [ a, b, c ]
            , fuzz4 string string string string "fuzz4" <|
                \a b c d ->
                    testStringLengthIsPreserved [ a, b, c, d ]
            , fuzz5 string string string string string "fuzz5" <|
                \a b c d e ->
                    testStringLengthIsPreserved [ a, b, c, d, e ]
            ]
        , fuzz
            (intRange 1 6)
            "intRange"
            (Expect.greaterThan 0)
        , fuzz
            (frequency [ ( 1, intRange 1 6 ), ( 1, intRange 1 20 ) ])
            "Fuzz.frequency"
            (Expect.greaterThan 0)
        , fuzz (result string int) "Fuzz.result" <| \r -> Expect.pass
        , fuzz (andThen (\i -> intRange 0 (2 ^ i)) (intRange 1 8))
            "Fuzz.andThen"
            (Expect.atMost 256)
        , fuzz
            (map2 (,) die die
                |> conditional
                    { retries = 10
                    , fallback = \( a, b ) -> ( a, (b + 1) % 6 )
                    , condition = \( a, b ) -> a /= b
                    }
            )
            "conditional: reroll dice until they are not equal"
          <|
            \( roll1, roll2 ) ->
                roll1 |> Expect.notEqual roll2
        , fuzz seed "conditional: shrunken values all pass condition" <|
            \seed ->
                let
                    evenInt : Fuzzer Int
                    evenInt =
                        Fuzz.intRange 0 10
                            |> Fuzz.conditional
                                { retries = 3
                                , fallback = (+) 1
                                , condition = even
                                }

                    even : Int -> Bool
                    even n =
                        (n % 2) == 0

                    shrinkable : Test.Runner.Shrinkable Int
                    shrinkable =
                        Test.Runner.fuzz evenInt
                            |> flip Random.step seed
                            |> Tuple.first
                            |> Tuple.second

                    testShrinkable : Test.Runner.Shrinkable Int -> Expect.Expectation
                    testShrinkable shrinkable =
                        case Test.Runner.shrink False shrinkable of
                            Nothing ->
                                Expect.pass

                            Just ( value, next ) ->
                                if even value then
                                    testShrinkable next

                                else
                                    Expect.fail <| "Shrunken value does not pass conditional: " ++ toString value
                in
                testShrinkable shrinkable
        , describe "Whitebox testing using Fuzz.Internal"
            [ fuzz randomSeedFuzzer "the same value is generated with and without shrinking" <|
                \seed ->
                    let
                        step gen =
                            Random.step gen seed

                        aFuzzer =
                            tuple5
                                ( tuple ( list int, array float )
                                , maybe bool
                                , result unit char
                                , tuple3
                                    ( percentage
                                    , map2 (+) int int
                                    , frequency [ ( 1, constant True ), ( 3, constant False ) ]
                                    )
                                , tuple3 ( intRange 0 100, floatRange -51 pi, map abs int )
                                )

                        valNoShrink =
                            aFuzzer |> Result.map (Random.map RoseTree.root >> step >> Tuple.first)

                        valWithShrink =
                            aFuzzer |> Result.map (step >> Tuple.first >> RoseTree.root)
                    in
                    Expect.equal valNoShrink valWithShrink
            , shrinkingTests
            , manualFuzzerTests
            ]
        , unicodeStringFuzzerTests
        ]


shrinkingTests : Test
shrinkingTests =
    let
    -- To test shrinking, we have to fail some tests so we can shrink their inputs.
    -- The best place we found for storing the expected last state(s) of the shrinking procedure is the description field, which is why we have this function here.
    -- Previously, we (ab)used Expect.true for this, but since that was removed, here we are.
        expectTrueAndExpectShrinkResultToEqualString label a =
            Expect.equal True a |> Expect.onFail label
    in
    testShrinking <|
        describe "tests that fail intentionally to test shrinking"
            [ fuzz2 int int "Every pair of ints has a zero" <|
                \i j ->
                    (i == 0)
                        || (j == 0)
                        |> expectTrueAndExpectShrinkResultToEqualString "(1,1)"
            , fuzz3 int int int "Every triple of ints has a zero" <|
                \i j k ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        |> expectTrueAndExpectShrinkResultToEqualString "(1,1,1)"
            , fuzz4 int int int int "Every 4-tuple of ints has a zero" <|
                \i j k l ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        || (l == 0)
                        |> expectTrueAndExpectShrinkResultToEqualString "(1,1,1,1)"
            , fuzz5 int int int int int "Every 5-tuple of ints has a zero" <|
                \i j k l m ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        || (l == 0)
                        || (m == 0)
                        |> expectTrueAndExpectShrinkResultToEqualString "(1,1,1,1,1)"
            , fuzz (list int) "All lists are sorted" <|
                \aList ->
                    let
                        checkPair l =
                            case l of
                                a :: b :: more ->
                                    if a > b then
                                        False

                                    else
                                        checkPair (b :: more)

                                _ ->
                                    True
                    in
                    checkPair aList |> expectTrueAndExpectShrinkResultToEqualString "[1,0]|[0,-1]"
            , fuzz (intRange 1 8 |> andThen (\i -> intRange 0 (2 ^ i))) "Fuzz.andThen shrinks a number" <|
                \i ->
                    i <= 2 |> expectTrueAndExpectShrinkResultToEqualString "3"
            ]


type alias ShrinkResult a =
    Maybe ( a, Test.Runner.Shrinkable a )


manualFuzzerTests : Test
manualFuzzerTests =
    describe "Test.Runner.{fuzz, shrink}"
        [ fuzz randomSeedFuzzer "Claim there are no even numbers" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce an even number
                    fuzzer =
                        Fuzz.intRange 2 10000
                            |> Fuzz.map
                                (\n ->
                                    if failsTest n then
                                        n

                                    else
                                        n + 1
                                )

                    failsTest n =
                        n % 2 == 0

                    pair =
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, shrinkN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.shrink False shrinkN)

                                else
                                    unfold acc (Test.Runner.shrink True shrinkN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.equal True >> Expect.onFail "Not all elements were even"
                        , List.head
                            >> Maybe.map (Expect.all [ Expect.lessThan 5, Expect.atLeast 0 ])
                            >> Maybe.withDefault (Expect.fail "Did not cause failure")
                        , List.reverse >> List.head >> Expect.equal (Maybe.map Tuple.first pair)
                        ]
        , fuzz randomSeedFuzzer "No strings contain the letter e" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce a string with the letter e
                    fuzzer =
                        map2 (\pre suf -> pre ++ "e" ++ suf) string string

                    failsTest =
                        String.contains "e"

                    pair =
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, shrinkN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.shrink False shrinkN)

                                else
                                    unfold acc (Test.Runner.shrink True shrinkN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.equal True >> Expect.onFail "Not all contained the letter e"
                        , List.head >> Expect.equal (Just "e")
                        , List.reverse >> List.head >> Expect.equal (Maybe.map Tuple.first pair)
                        ]
        , fuzz randomSeedFuzzer "List shrinker finds the smallest counter example" <|
            \seed ->
                let
                    fuzzer : Fuzzer (List Int)
                    fuzzer =
                        Fuzz.list Fuzz.int

                    allEven : List Int -> Bool
                    allEven xs =
                        List.all (\x -> x % 2 == 0) xs

                    initialShrink : ShrinkResult (List Int)
                    initialShrink =
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    shrink : Maybe (List Int) -> ShrinkResult (List Int) -> Maybe (List Int)
                    shrink shrunken lastShrink =
                        case lastShrink of
                            Just ( valN, shrinkN ) ->
                                shrink
                                    (if allEven valN then
                                        shrunken

                                     else
                                        Just valN
                                    )
                                    (Test.Runner.shrink (allEven valN) shrinkN)

                            Nothing ->
                                shrunken
                in
                case shrink Nothing initialShrink of
                    Just shrunken ->
                        Expect.equal [ 1 ] shrunken

                    Nothing ->
                        Expect.pass
        ]


unicodeStringFuzzerTests : Test
unicodeStringFuzzerTests =
    describe "unicode string fuzzer"
        [ expectToFail <|
            fuzz string "generates ascii" <|
                \str -> str |> String.contains "E" |> Expect.true "Expected to find ascii letter E"
        , expectToFail <|
            fuzz string "generates whitespace" <|
                \str -> str |> String.contains "\t" |> Expect.true "Expected to find a tab character"
        , expectToFail <|
            fuzz string "generates combining diacritical marks" <|
                \str -> str |> String.contains "̃" |> Expect.true "Expected to find the combining diactricial mark character tilde"
        , expectToFail <|
            fuzz string "generates emoji" <|
                \str -> str |> String.contains "🔥" |> Expect.true "Expected to find 🔥 emoji"
        , expectToFail <|
            fuzz string "generates long strings with a single character" <|
                \str ->
                    let
                        countSequentialUniquesAtStart s =
                            case s of
                                a :: b :: cs ->
                                    if a == b then
                                        1 + countSequentialUniquesAtStart (b :: cs)
                                    else
                                        0

                                _ ->
                                    0
                    in
                    str |> String.toList |> countSequentialUniquesAtStart |> (\x -> x < 7) |> Expect.true "expected a string with 7-length duplicates"
        ]
