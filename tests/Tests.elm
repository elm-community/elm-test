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
import Float


all : Test
all =
    Test.concat
        [ readmeExample, regressions, expectationTests, fuzzerTests ]


readmeExample : Test
readmeExample =
    describe "the String module"
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
        , describe "Expect.within and Expect.notWithin" <|
            {-
               What you want to know before reading these tests:
                   There's a huge amount of small nuances in implementing and testing a general purpose
                   floating point comparison algorithm. This is reflected in the size and complexity of
                   these unit tests.

                   In many places we use e.g. 0.501 or 0.499 instead of (1/2); this is because we need
                   some tolerance of precision loss caused by the comparison algorithm. This tolerance
                   is brute force binary search tested; if the tests ever fail, increase the relevant
                   tolerance a bit. There are also some gaps between notWithin and within in some fuzz
                   tests; this is again because of tolerance.

                   Despite all this effort, this will not cover all use-cases. The only way to get
                   perfect tolerance is to analyze the floating point operations you are doing in your
                   own program, and calculate what the maximum tolerance, relative and absolute, should
                   be for your specific run. This also differs based on what numbers you are running
                   through your algorithm. We do however expect this to handle all use-cases for almost
                   all developers. If you're relying on high precision in denormalized floats, you
                   might have to implement a solution specific to your problem.

                Want we want to test:

                    - some getting-up-to-speed test
                        - pythagorean identity, pi

                    - negative/positive number combinations of all below:
                        - U near-inf
                        - UF edge large - small
                        - UF edge small - abs
                        - UF edge abs - zero
                        - UF edge abs - negative abs
                        - U test cases in documentation
                        - U infinity equals self
                        - F infinity does not equal finite numbers
                        - U NaN does not equal self
                        - F NaN does not equal anything
                        - F reflexivity, no matter what positive epsilon is used
                        - U zero tolerance reflexivity
                        - F zero tolerance does not equal anything other than self
                        - U zero tolerance removes abs check
                        - U zero signed/unsigned comparison
                        - F negative tolerance matches nothing
                        - F smoke large
                        - F smoke small
                        - F smoke abs

                   at least 47 tests, 3 of which are for documentation and one is a demo test

            -}
            [ describe "examples in documentation"
                [ fuzz float "pythagorean identity" <|
                    -- U getting started-test
                    \x ->
                        (sin x) ^ 2 + (cos x) ^ 2 |> Expect.within Float.epsilon 1.0
                  -- U test cases in documentation
                , test "floats known to not add exactly" <|
                    -- (0.1 is non-terminating in base 2)
                    \() -> 0.1 + 0.2 |> Expect.within Float.epsilon 0.3
                , test "somewhat accurate approximation of pi" <|
                    \() -> 3.14 |> Expect.within 0.01 pi
                , test "less accurate approximation of pi" <|
                    \() -> 3.1 |> Expect.notWithin 0.01 pi
                ]
            , describe "infinity"
                [ fuzz float "infinity equality" <|
                    -- U infinity equals self
                    \epsilon ->
                        Float.infinity |> Expect.within (abs epsilon) Float.infinity
                , fuzz float "negative infinity equality" <|
                    -- U negative infinity equals self
                    \epsilon ->
                        -Float.infinity |> Expect.within (abs epsilon) -Float.infinity
                , fuzz float "Float.infinity does not equal any finite number" <|
                    -- F infinity does not equal finite numbers
                    \a -> Float.infinity |> Expect.notWithin Float.epsilon a
                  -- U infinity does not equal the nearest finite floats, cartesian product positive/negative
                , test "infinity does not equal the largest finite float" <|
                    \() -> Float.infinity |> Expect.notWithin Float.minAbsValue Float.maxAbsValue
                , test "infinity does not equal the smallest finite float" <|
                    \() -> Float.infinity |> Expect.notWithin Float.minAbsValue -Float.maxAbsValue
                , test "negative infinity does not equal the largest finite float" <|
                    \() -> -Float.infinity |> Expect.notWithin Float.minAbsValue Float.maxAbsValue
                , test "negative infinity does not equal the smallest finite float" <|
                    \() -> -Float.infinity |> Expect.notWithin Float.minAbsValue -Float.maxAbsValue
                ]
            , describe "NaN"
                [ fuzz2 float float "NaN does not equal anything" <|
                    -- F NaN does not equal anything
                    \epsilon a ->
                        a |> Expect.notWithin (abs epsilon) Float.nan
                , fuzz float "NaN does not equal NaN" <|
                    -- F NaN does not equal self
                    \epsilon ->
                        Float.nan |> Expect.notWithin (abs epsilon) Float.nan
                ]
            , describe "zero tolerance"
                [ fuzz float "self-equality with zero tolerance" <|
                    -- U zero epsilon equals self
                    \a ->
                        a |> Expect.within 0 a
                , fuzz2 float float "self-equality with zero tolerance" <|
                    -- F zero epsilon does not equal anything other than self
                    \a b ->
                        if a == b then
                            a |> Expect.within 0 b
                        else
                            a |> Expect.notWithin 0 b
                , test "values extremely close to zero are not considered equal when tolerance is 0" <|
                    -- U zero epsilon removes abs check
                    \() -> Float.minAbsValue |> Expect.notWithin 0 0
                ]
            , describe "negative tolerance"
                [ fuzz3 float float float "nothing is nearly equal when tolerance is negative" <|
                    -- F negative tolerance matches nothing
                    \epsilon a b ->
                        if epsilon == 0 then
                            Expect.pass
                        else
                            Expect.notWithin -(abs epsilon) a b
                ]
            , describe "edges between comparison algorithms used internally in Expect.within"
                [ describe "edge at Float.infinity"
                    [ test "very large float equality" <|
                        -- U edge inf - large
                        \() ->
                            -- subtract smallest representable double from Float.maxValue
                            Float.maxAbsValue |> Expect.within 1 (Float.maxAbsValue - (8.98846567431 * 10 ^ 307))
                    , test "very large float equals Float.infinity with high enough tolerance" <|
                        -- U edge inf - large
                        \() ->
                            Float.maxAbsValue |> Expect.within Float.epsilon Float.infinity
                    ]
                , describe "edge at Float.minAbsNormal"
                    [ test "plus minus minNormal equality" <|
                        -- this is right on the edge between relative and absolute comparison
                        -- it should be a fairly smooth transition
                        -- U edge large - small
                        \() ->
                            Expect.all
                                -- just under the relative edge
                                [ (\d -> (d / 2.0) |> Expect.notWithin (4 / 3) (-d / 2.0))
                                  -- note: floats have such low precision down here that 1.333... is the closest factor below 2 we can use.
                                  -- a factor such as 1.5 or 1.999 causes a rounding error internally in `Expect.within`.
                                , (\d -> (d / 2.0) |> Expect.within 1.333333333333334 (-d / 2.0))
                                , (\d -> (d / 2.0) |> Expect.within 2 (-d / 2.0))
                                , (\d -> (d / 2.0) |> Expect.within 2.001 (-d / 2.0))
                                  -- on the relative/absolute edge
                                , (\d -> d |> Expect.notWithin 1.999 -d)
                                , (\d -> d |> Expect.within 2 -d)
                                , (\d -> d |> Expect.within 2.001 -d)
                                  -- just over the relative edge
                                , (\d -> 2 * d |> Expect.notWithin 1.999 (-d * 2.0))
                                , (\d -> 2 * d |> Expect.within 2 (-d * 2.0))
                                , (\d -> 2 * d |> Expect.within 2.001 (-d * 2.0))
                                ]
                                Float.minAbsNormal
                    , test "float equality on minNormal edge" <|
                        \() -> Float.minAbsNormal |> Expect.within Float.epsilon Float.minAbsNormal
                    , test "plus minus float equality on minNormal edge" <|
                        \() -> Float.minAbsNormal |> Expect.within 2 -Float.minAbsNormal
                    ]
                , describe "edge at Float.minAbsValue * 2^4, below which we only compare absolute values"
                    -- F edge small - abs
                    [ fuzz (intRange 1 (2 ^ 15)) "Plus-minus epsilon equality" <|
                        -- Intended to verify that comparison of numbers below Float.minNormal are less exact.
                        \mult ->
                            let
                                -- variable such that all test values are below Float.minNormal
                                low =
                                    (toFloat mult) * Float.minAbsNormal * 2 ^ -16

                                -- variable such that all test values are above Float.minNormal
                                high =
                                    (toFloat mult) * Float.minAbsNormal * 2 ^ 16
                            in
                                Expect.all
                                    [ (\( low, high ) -> low * 1.1 |> Expect.within 0.10001 low)
                                    , (\( low, high ) -> high * 1.1 |> Expect.within 0.10001 high)
                                    , (\( low, high ) -> low * 1.1 |> Expect.within (1 - 1 / 1.0999999) low)
                                    , (\( low, high ) -> high * 1.1 |> Expect.notWithin (1 - 1 / 1.0999999) high)
                                    ]
                                    ( low, high )
                      -- U edge small - abs
                    , fuzz (floatRange (Float.minAbsValue) (Float.minAbsValue * 2 ^ 4)) "absolute comparison for values below edge at Float.minAbsValue * 2^4" <|
                        \a ->
                            if abs a < Float.minAbsValue * 2 ^ 4 then
                                -- absolute comparison
                                Expect.all
                                    [ (\a -> a |> Expect.within Float.minAbsValue a)
                                    , (\a -> a |> Expect.within Float.minAbsValue -a)
                                    , (\a -> a |> Expect.within Float.minAbsValue 0)
                                    , (\a -> a |> Expect.within 1 0)
                                    , (\a -> a |> Expect.within 1 -a)
                                    , (\a -> a |> Expect.within 2 -a)
                                    ]
                                    a
                            else
                                -- relative comparison
                                Expect.all
                                    [ (\a -> a |> Expect.within Float.minAbsValue a)
                                    , (\a -> a |> Expect.notWithin Float.minAbsValue -a)
                                    , (\a -> a |> Expect.notWithin Float.minAbsValue 0)
                                    , (\a -> a |> Expect.within 1 0)
                                    , (\a -> a |> Expect.within 1 -a)
                                    , (\a -> a |> Expect.within 2 -a)
                                    ]
                                    a
                    , describe "edge at 0"
                        -- UF edge abs - zero
                        -- UF edge abs - negative abs
                        [ -- U zero signed/unsigned comparison
                          fuzz float "zero equality" <|
                            \epsilon -> 0.0 |> Expect.within (abs epsilon) 0.0
                        , fuzz float "zero equality, signed" <|
                            \epsilon -> 0.0 |> Expect.within (abs epsilon) -0.0
                        , fuzz2 float (floatRange Float.minAbsNormal (2 ^ 44 * Float.minAbsNormal)) "near-zero self equality" <|
                            -- F zero signed/unsigned comparison
                            -- intended to test absolute comparison (the near-zero case)
                            \epsilon a ->
                                a |> Expect.within (abs epsilon) a
                          --
                          -- tests for comparison of floats small enough to be considered zero
                        , test "extremely small float equality" <|
                            \() -> Float.minAbsValue |> Expect.within Float.epsilon Float.minAbsValue
                        , test "extremely small plus minus float equality" <|
                            \() -> Float.minAbsValue |> Expect.within Float.epsilon -Float.minAbsValue
                        , test "extremely small float equality, abs tolerance" <|
                            \() -> Float.minAbsValue |> Expect.within Float.epsilon (4 * Float.minAbsValue)
                        , test "extremely small plus minus float equality, abs tolerance" <|
                            \() -> Float.minAbsValue |> Expect.within Float.epsilon (-4 * Float.minAbsValue)
                        ]
                    ]
                , fuzz float "plus minus epsilon equality for larger numbers" <|
                    -- intended to test the relative comparison
                    \a ->
                        -- 3 * minAbsNormal is slightly larger than the highest tolerance used
                        if abs a <= 3 * Float.minAbsNormal then
                            Expect.pass
                        else
                            Expect.all
                                [ (\d -> d |> Expect.notWithin 1.999 -d)
                                , (\d -> d |> Expect.within 2 -d)
                                , (\d -> d |> Expect.within 2.001 -d)
                                ]
                                a
                , describe "Float.minNormal to Float.infinity"
                    -- F smoke large
                    [ fuzz (floatRange Float.minAbsNormal (Float.maxAbsValue / 2)) "Compares multiplicatively within, large values" <|
                        \f -> f |> Expect.within 1 (2 * f)
                    , fuzz (floatRange Float.minAbsNormal (Float.maxAbsValue / 2)) "Compares multiplicatively notWithin, large values" <|
                        \f -> f |> Expect.notWithin 0.999 (2 * f)
                    ]
                , describe "Float.minAbsValue * 2^4 to Float.minNormal"
                    -- F smoke small
                    -- Gap between 2/3 and 1/2 is due to the scaling of the comparison within this floatRange.
                    [ fuzz (floatRange (Float.minAbsValue * (2 ^ 4) * 4) (Float.minAbsNormal / 4)) "Compares multiplicatively within, small values" <|
                        \f -> f |> Expect.within 0.667 (2 * f)
                    , fuzz (floatRange (Float.minAbsValue * (2 ^ 4) * 4) (Float.minAbsNormal / 4)) "Compares multiplicatively notWithin, small values" <|
                        \f -> f |> Expect.notWithin 0.49 (2 * f)
                    ]
                , describe "zero to positive/negative Float.minAbsValue * 2^4"
                    -- F smoke abs
                    [ fuzz (floatRange (-Float.minAbsValue * (2 ^ 4) / 2) (Float.minAbsValue * (2 ^ 4) / 2)) "Compares multiplicatively within, near zero" <|
                        \f -> f |> Expect.within 0.5000001 (2 * f)
                    , fuzz (floatRange (-Float.minAbsValue * (2 ^ 4) / 2) (Float.minAbsValue * (2 ^ 4) / 2)) "Compares multiplicatively notWithin, near zero" <|
                        \f ->
                            if f == 0 then
                                Expect.pass
                            else
                                f |> Expect.notWithin 0 (2 * f)
                    ]
                , describe "algebraic properties"
                    -- F within = not notWithin
                    [ fuzz4 float float float float "Within = not notWithin" <|
                        \epsilon a b delta ->
                            let
                                isWithin =
                                    Expect.within delta a b

                                isNotWithin =
                                    Expect.notWithin delta a b
                            in
                                Expect.notEqual (succeeded isWithin) (succeeded isNotWithin)
                      -- F commutativity
                    , fuzz3 float float float "within commutativity" <|
                        \epsilon a b ->
                            succeeded (Expect.within (abs epsilon) a b) |> Expect.equal (succeeded <| Expect.within (abs epsilon) b a)
                    , fuzz3 float float float "notWithin commutativity" <|
                        \epsilon a b ->
                            succeeded (Expect.notWithin (abs epsilon) a b) |> Expect.equal (succeeded <| Expect.notWithin (abs epsilon) b a)
                      -- F reflexivity, no matter what positive epsilon is used
                    , fuzz2 float float "within reflexive" <|
                        \epsilon a ->
                            Expect.within (abs epsilon) a a
                      -- this next test should always fail
                    , expectToFail <|
                        fuzz2 float float "notWithin irreflexive" <|
                            \epsilon a ->
                                Expect.notWithin (abs epsilon) a a
                    ]
                , describe "smoke tests"
                    [ test "large difference float equality" <|
                        \() -> 2 ^ 1000 |> Expect.notWithin 1 -Float.minAbsNormal
                    ]
                ]
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
        describe "tests that fail intentionally to test shrinking"
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
    describe "Test.Runner.{fuzz, shrink}"
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
                    -- fuzzer is guaranteed to produce a string with the letter e
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
