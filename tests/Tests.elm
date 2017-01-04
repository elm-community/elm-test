module Tests exposing (all)

import Test exposing (..)
import Test.Expectation exposing (Expectation(..))
import Test.Internal as TI
import Test.Runner
import Fuzz exposing (..)
import Dict
import Set
import String
import Expect
import Fuzz.Internal
import RoseTree
import Random.Pcg as Random
import Shrink
import Lazy.List as LL
import Helpers exposing (..)


all : Test
all =
    Test.concat
        [ readmeExample, regressions, expectationTests, fuzzerTests ]


readmeExample : Test
readmeExample =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \() ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)
            , test "reverses a known string" <|
                \() ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , test "equal lists" <|
                \() ->
                    [ 1, 2, 3 ]
                        |> Expect.equalLists [ 1, 2, 3 ]
            , test "equal dicts" <|
                \() ->
                    (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])
                        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])
            , test "equal sets" <|
                \() ->
                    (Set.fromList [ 1, 2, 3 ])
                        |> Expect.equalSets (Set.fromList [ 1, 2, 3 ])
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


expectationTests : Test
expectationTests =
    describe "Expectations"
        [ describe "Expect.err"
            [ test "passes on Err _" <|
                \() ->
                    Err 12 |> Expect.err
            , expectToFail <|
                test "passes on Ok _" <|
                    \() ->
                        Ok 12 |> Expect.err
            ]
        , describe "Expect.within" <|
            -- these constants are relevant for all float operations; are they available in some core float module?
            let
                -- largest possible rounding error, for implementations following IEEE 754-2008.
                -- according to MSDN system.double.epsilon, arm has an epsilon that is too small to fit
                -- in a 64bit float, so we're just going to ignore that one.
                float64epsilon =
                    2.0 ^ -52

                infinity =
                    1.0 / 0.0

                -- Not a Number
                nan =
                    0.0 / 0.0

                -- Smallest possible value before we start losing precision
                float64minNormal =
                    2.0 ^ -1022

                -- Largest finite value representable in a 64bit float
                float64maxValue =
                    (2.0 - float64epsilon) * 2.0 ^ 1023

                -- Smallest finite positive value representable in a 64bit float
                float64minValue =
                    2.0 ^ -1074
            in
                [ fuzz float "pythagorean identity" <|
                    \x ->
                        (sin x) ^ 2 + (cos x) ^ 2 |> Expect.within float64epsilon 1.0
                , test "floats known to not add exactly" <|
                    \() -> 0.1 + 0.2 |> Expect.within float64epsilon 0.3
                , test "somewhat accurate approximation of pi" <|
                    \() -> 3.14 |> Expect.within 0.01 pi
                , fuzz2 float float "self equality" <|
                    \epsilon value ->
                        value |> Expect.within (abs epsilon) value
                , fuzz float "NaN non-equality" <|
                    \epsilon ->
                        nan |> Expect.notWithin (abs epsilon) nan
                , fuzz float "Infinity equality" <|
                    \epsilon ->
                        infinity |> Expect.within (abs epsilon) infinity
                , fuzz float "Negative infinity equality" <|
                    \epsilon ->
                        -infinity |> Expect.within (abs epsilon) -infinity
                , fuzz float "Zero equality" <|
                    \epsilon -> 0.0 |> Expect.within (abs epsilon) 0.0
                , fuzz float "Zero equality, signed" <|
                    \epsilon -> 0.0 |> Expect.within (abs epsilon) -0.0
                , fuzz2 float int "Near-zero equality" <|
                    -- int argument to limit range of variable
                    -- intended to test the absolute comparison (the near-zero case)
                    \epsilon a ->
                        let
                            da =
                                10.0 ^ -305 * toFloat a
                        in
                            da |> Expect.within (abs epsilon) da
                , fuzz float "Plus-minus epsilon equality for larger numbers" <|
                    -- intended to test the relative comparison
                    \a ->
                        -- 3 is slightly larger than the highest tolerance used
                        if abs a <= 3 * float64minNormal then
                            Expect.pass
                        else
                            Expect.all
                                [ (\d -> d |> Expect.notWithin 1.999 -d)
                                , (\d -> d |> Expect.within 2 -d)
                                , (\d -> d |> Expect.within 2.001 -d)
                                ]
                                a
                , fuzz (intRange 1 (2 ^ 15)) "Plus-minus epsilon equality" <|
                    -- Int argument to limit range of variable.
                    -- Intended to test less exact comparison of numbers below float64minNormal.
                    \mult ->
                        let
                            -- variable such that all test values are below float64minNormal
                            low =
                                (toFloat mult) * float64minNormal * 2 ^ -16

                            -- variable such that all test values are above float64minNormal
                            high =
                                (toFloat mult) * float64minNormal * 2 ^ 16
                        in
                            -- a has higher tolerance than b, so b + 10% should not be within b + 9.9%, but the inverse for a
                            Expect.all
                                [ (\( low, high ) -> low * 1.1 |> Expect.within 0.10001 low)
                                , (\( low, high ) -> high * 1.1 |> Expect.within 0.10001 high)
                                , (\( low, high ) -> low * 1.1 |> Expect.within (1 - 1 / 1.0999999) low)
                                , (\( low, high ) -> high * 1.1 |> Expect.notWithin (1 - 1 / 1.0999999) high)
                                ]
                                ( low, high )
                , test "Plus minus minNormal equality" <|
                    -- this is right on the edge between relative and absolute comparison
                    -- it should be a smooth transition
                    \() ->
                        Expect.all
                            -- just under the relative edge
                            [ (\d -> (d / 2.0) |> Expect.notWithin 1.999 -(d / 2.0))
                            , (\d -> (d / 2.0) |> Expect.within 2 -(d / 2.0))
                            , (\d -> (d / 2.0) |> Expect.within 2.001 -(d / 2.0))
                              -- on the relative/absolute edge
                            , (\d -> d |> Expect.notWithin 1.999 -d)
                            , (\d -> d |> Expect.within 2 -d)
                            , (\d -> d |> Expect.within 2.001 -d)
                              -- just over the relative edge
                            , (\d -> 2 * d |> Expect.notWithin 1.999 (-d * 2.0))
                            , (\d -> 2 * d |> Expect.within 2 (-d * 2.0))
                            , (\d -> 2 * d |> Expect.within 2.001 (-d * 2.0))
                            ]
                            float64minNormal
                , test "Very large float equality" <|
                    \() ->
                        -- subtract smallest representable double from float64maxValue
                        float64maxValue |> Expect.within 1 (float64maxValue - (8.98846567431 * 10 ^ 307))
                , test "Very small float equality" <|
                    \() -> float64minNormal |> Expect.within 1 float64minNormal
                , test "Very small plus minus float equality" <|
                    \() -> float64minNormal |> Expect.within 1 float64minNormal
                , test "Very large difference float equality" <|
                    \() -> 2.0 ^ 1000 |> Expect.notWithin 1 -float64minNormal
                , fuzz4 float float float float "Within = not notWithin" <|
                    \epsilon a b delta ->
                        let
                            isWithin =
                                Expect.within delta a b

                            isNotWithin =
                                Expect.notWithin delta a b
                        in
                            Expect.notEqual (succeeded isWithin) (succeeded isNotWithin)
                , fuzz3 float float float "within commutativity" <|
                    \epsilon a b ->
                        succeeded (Expect.within (abs epsilon) a b) |> Expect.equal (succeeded <| Expect.within (abs epsilon) b a)
                , fuzz3 float float float "notWithin commutativity" <|
                    \epsilon a b ->
                        succeeded (Expect.notWithin (abs epsilon) a b) |> Expect.equal (succeeded <| Expect.notWithin (abs epsilon) b a)
                , fuzz2 float float "within reflexive" <|
                    \epsilon a ->
                        Expect.within (abs epsilon) a a
                , fuzz3 float float float "nothing is nearly equal when tolerance is negative" <|
                    \epsilon a b ->
                        if epsilon == 0 then
                            Expect.pass
                        else
                            Expect.notWithin -(abs epsilon) a b
                  -- this next test should always fail
                , expectToFail <|
                    fuzz2 float float "notWithin irreflexive" <|
                        \epsilon a ->
                            Expect.notWithin (abs epsilon) a a
                ]
        ]


regressions : Test
regressions =
    describe "regression tests"
        [ fuzz (intRange 1 32) "for #39" <|
            \positiveInt ->
                positiveInt
                    |> Expect.greaterThan 0
        ]


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
            (frequencyOrCrash [ ( 1, intRange 1 6 ), ( 1, intRange 1 20 ) ])
            "Fuzz.frequency(OrCrash)"
            (Expect.greaterThan 0)
        , fuzz (result string int) "Fuzz.result" <| \r -> Expect.pass
        , fuzz (andThen (\i -> intRange 0 (2 ^ i)) (intRange 1 8))
            "Fuzz.andThen"
            (Expect.atMost 256)
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
                                    , frequencyOrCrash [ ( 1, constant True ), ( 3, constant False ) ]
                                    )
                                , tuple3 ( intRange 0 100, floatRange -51 pi, map abs int )
                                )

                        valNoShrink =
                            aFuzzer |> Fuzz.Internal.unpackGenVal |> step |> Tuple.first

                        valWithShrink =
                            aFuzzer |> Fuzz.Internal.unpackGenTree |> step |> Tuple.first |> RoseTree.root
                    in
                        Expect.equal valNoShrink valWithShrink
            , shrinkingTests
            , manualFuzzerTests
            ]
        ]


shrinkingTests : Test
shrinkingTests =
    testShrinking <|
        describe "Tests that fail intentionally to test shrinking"
            [ fuzz2 int int "Every pair of ints has a zero" <|
                \i j ->
                    (i == 0)
                        || (j == 0)
                        |> Expect.true "(1,1)"
            , fuzz3 int int int "Every triple of ints has a zero" <|
                \i j k ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        |> Expect.true "(1,1,1)"
            , fuzz4 int int int int "Every 4-tuple of ints has a zero" <|
                \i j k l ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        || (l == 0)
                        |> Expect.true "(1,1,1,1)"
            , fuzz5 int int int int int "Every 5-tuple of ints has a zero" <|
                \i j k l m ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        || (l == 0)
                        || (m == 0)
                        |> Expect.true "(1,1,1,1,1)"
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
                        checkPair aList |> Expect.true "[1,0]|[0,-1]"
            ]


manualFuzzerTests : Test
manualFuzzerTests =
    describe "Test Test.Runner.{fuzz, shrink}"
        [ fuzz randomSeedFuzzer "Claim there are no even numbers" <|
            \seed ->
                let
                    -- fuzzer is gauranteed to produce an even number
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
                        Random.step (Test.Runner.fuzz fuzzer) seed |> Tuple.first

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
                    unfold [] (Just pair)
                        |> Expect.all
                            [ List.all failsTest >> Expect.true "Not all elements were even"
                            , List.head
                                >> Maybe.map (Expect.all [ Expect.lessThan 5, Expect.atLeast 0 ])
                                >> Maybe.withDefault (Expect.fail "Did not cause failure")
                            , List.reverse >> List.head >> Expect.equal (Just (Tuple.first pair))
                            ]
        , fuzz randomSeedFuzzer "No strings contain the letter e" <|
            \seed ->
                let
                    -- fuzzer is gauranteed to produce a string with the letter e
                    fuzzer =
                        map2 (\pre suf -> pre ++ "e" ++ suf) string string

                    failsTest =
                        String.contains "e"

                    pair =
                        Random.step (Test.Runner.fuzz fuzzer) seed |> Tuple.first

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
                    unfold [] (Just pair)
                        |> Expect.all
                            [ List.all failsTest >> Expect.true "Not all contained the letter e"
                            , List.head >> Expect.equal (Just "e")
                            , List.reverse >> List.head >> Expect.equal (Just (Tuple.first pair))
                            ]
        ]
