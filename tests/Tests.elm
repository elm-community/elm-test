module Tests exposing (expectationTests, identicalNamesAreRejectedTests, readmeExample, regressions, testTests, withinTests)

import Dict
import Expect
import Expect exposing (FloatingPointTolerance(Absolute, AbsoluteOrRelative, Relative))
import Fuzz exposing (..)
import Fuzz.Internal
import FuzzerTests exposing (fuzzerTests)
import Helpers exposing (..)
import Random.Pcg as Random
import RoseTree
import RunnerTests
import Set
import Shrink
import String
import Test exposing (..)
import Test.Expectation exposing (Expectation(..))
import Test.Runner


readmeExample : Test
readmeExample =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
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
                \_ ->
                    Err 12 |> Expect.err
            , expectToFail <|
                test "passes on Ok _" <|
                    \_ ->
                        Ok 12 |> Expect.err
            ]
        , describe "Expect.all"
            [ expectToFail <|
                test "fails with empty list" <|
                    \_ -> "dummy subject" |> Expect.all []
            ]
        ]


same a b =
    case ( a, b ) of
        ( Test.Expectation.Pass, Test.Expectation.Pass ) ->
            Test.Expectation.Pass

        ( Test.Expectation.Fail _, Test.Expectation.Fail _ ) ->
            Test.Expectation.Pass

        ( a, b ) ->
            Test.Expectation.fail { description = "expected both arguments to fail, or both to succeed", reason = Test.Expectation.Equals (toString a) (toString b) }


different a b =
    case ( a, b ) of
        ( Test.Expectation.Pass, Test.Expectation.Fail _ ) ->
            Test.Expectation.Pass

        ( Test.Expectation.Fail _, Test.Expectation.Pass ) ->
            Test.Expectation.Pass

        ( a, b ) ->
            Test.Expectation.fail { description = "expected one argument to fail", reason = Test.Expectation.Equals (toString a) (toString b) }


withinTests =
    describe "Expect.within"
        [ describe "use-cases"
            [ fuzz float "pythagorean identity" <|
                \x ->
                    (sin x) ^ 2 + (cos x) ^ 2 |> Expect.within (AbsoluteOrRelative 0.000001 0.00001) 1.0
            , test "floats known to not add exactly" <|
                \_ -> 0.1 + 0.2 |> Expect.within (Absolute 0.000000001) 0.3
            , test "approximation of pi" <|
                \_ -> 3.14 |> Expect.within (Absolute 0.01) pi
            , fuzz (floatRange 0.000001 100000) "relative tolerance of circle circumference using pi approximation" <|
                \radius ->
                    (radius * pi)
                        |> Expect.within (Relative 0.001) (radius * 3.14)
            , expectToFail <|
                test "approximation of pi is not considered too accurate" <|
                    \_ -> 3.14 |> Expect.within (Absolute 0.001) pi
            , expectToFail <|
                fuzz (floatRange 0.000001 100000) "too high absolute tolerance of circle circumference using pi approximation" <|
                    \radius ->
                        (radius * pi)
                            |> Expect.within (Absolute 0.001) (radius * 3.14)
            , expectToFail <|
                fuzz (floatRange 0.000001 100000) "too high relative tolerance of circle circumference using pi approximation" <|
                    \radius ->
                        (radius * pi)
                            |> Expect.within (Relative 0.0001) (radius * 3.14)
            ]
        , describe "edge-cases"
            [ fuzz2 float float "self equality" <|
                \epsilon value ->
                    let
                        eps =
                            if epsilon /= 0 then
                                epsilon
                            else
                                1
                    in
                        value |> Expect.within (Relative (abs eps)) value
            , fuzz float "NaN equality" <|
                \epsilon ->
                    let
                        nan =
                            0.0 / 0.0
                    in
                        nan |> Expect.notWithin (Relative (abs epsilon)) nan
            , fuzz float "Infinity equality" <|
                \epsilon ->
                    let
                        infinity =
                            1.0 / 0.0
                    in
                        infinity |> Expect.within (Relative (abs epsilon)) infinity
            , fuzz float "Negative infinity equality" <|
                \epsilon ->
                    let
                        negativeInfinity =
                            -1.0 / 0.0
                    in
                        negativeInfinity |> Expect.within (Relative (abs epsilon)) negativeInfinity
            , fuzz3 float float float "within and notWithin should never agree on relative tolerance" <|
                \epsilon a b ->
                    let
                        withinTest =
                            a |> Expect.within (Relative (abs epsilon)) b

                        notWithinTest =
                            a |> Expect.notWithin (Relative (abs epsilon)) b
                    in
                        different withinTest notWithinTest
            , fuzz3 float float float "within and notWithin should never agree on absolute tolerance" <|
                \epsilon a b ->
                    let
                        withinTest =
                            a |> Expect.within (Absolute (abs epsilon)) b

                        notWithinTest =
                            a |> Expect.notWithin (Absolute (abs epsilon)) b
                    in
                        different withinTest notWithinTest
            , fuzz4 float float float float "within and notWithin should never agree on absolute or relative tolerance" <|
                \absoluteEpsilon relativeEpsilon a b ->
                    let
                        withinTest =
                            a |> Expect.within (AbsoluteOrRelative (abs absoluteEpsilon) (abs relativeEpsilon)) b

                        notWithinTest =
                            a |> Expect.notWithin (AbsoluteOrRelative (abs absoluteEpsilon) (abs relativeEpsilon)) b
                    in
                        different withinTest notWithinTest
            , fuzz float "Zero equality" <|
                \epsilon -> 0.0 |> Expect.within (Relative (abs epsilon)) 0.0
            , fuzz3 float float float "within absolute commutativity" <|
                \epsilon a b ->
                    same (Expect.within (Absolute (abs epsilon)) a b) (Expect.within (Absolute (abs epsilon)) b a)
            , fuzz3 float float float "notWithin absolute commutativity" <|
                \epsilon a b ->
                    same (Expect.notWithin (Absolute (abs epsilon)) a b) (Expect.notWithin (Absolute (abs epsilon)) b a)
            , fuzz2 float float "within absolute reflexive" <|
                \epsilon a ->
                    Expect.within (Absolute (abs epsilon)) a a
            , fuzz3 float float float "within relative commutativity" <|
                \epsilon a b ->
                    same (Expect.within (Relative (abs epsilon)) a b) (Expect.within (Relative (abs epsilon)) b a)
            , fuzz3 float float float "notWithin relative commutativity" <|
                \epsilon a b ->
                    same (Expect.notWithin (Relative (abs epsilon)) a b) (Expect.notWithin (Relative (abs epsilon)) b a)
            , fuzz2 float float "within relative reflexive" <|
                \epsilon a ->
                    Expect.within (Relative (abs epsilon)) a a
            ]
        ]


regressions : Test
regressions =
    describe "regression tests"
        [ fuzz (intRange 1 32) "for #39" <|
            \positiveInt ->
                positiveInt
                    |> Expect.greaterThan 0
        , fuzz
            (custom (Random.int 1 8) Shrink.noShrink)
            "fuzz tests run 100 times"
            (Expect.notEqual 5)
            |> expectToFail
          {- If fuzz tests actually run 100 times, then asserting that no number
             in 1..8 equals 5 fails with 0.999998 probability. If they only run
             once, or stop after a duplicate due to #127, then it's much more
             likely (but not guaranteed) that the 5 won't turn up. See #128.
          -}
        ]


testTests : Test
testTests =
    describe "functions that create tests"
        [ describe "describe"
            [ expectToFail <| describe "fails with empty list" []
            , expectToFail <| describe "" [ test "describe with empty description fail" expectPass ]
            ]
        , describe "test"
            [ expectToFail <| test "" expectPass
            ]
        , describe "fuzz"
            [ expectToFail <| fuzz Fuzz.bool "" expectPass
            ]
        , describe "fuzzWith"
            [ expectToFail <| fuzzWith { runs = 0 } Fuzz.bool "nonpositive" expectPass
            , expectToFail <| fuzzWith { runs = 1 } Fuzz.bool "" expectPass
            ]
        , describe "Test.todo"
            [ expectToFail <| todo "a TODO test fails"
            , test "Passes are not TODO"
                (\_ -> Expect.pass |> Test.Runner.isTodo |> Expect.false "was true")
            , test "Simple failures are not TODO" <|
                \_ ->
                    Expect.fail "reason" |> Test.Runner.isTodo |> Expect.false "was true"
            , test "Failures with TODO reason are TODO" <|
                \_ ->
                    Test.Expectation.fail { description = "", reason = Test.Expectation.TODO }
                        |> Test.Runner.isTodo
                        |> Expect.true "was false"
            ]
        , identicalNamesAreRejectedTests
        ]


identicalNamesAreRejectedTests : Test
identicalNamesAreRejectedTests =
    describe "Identically-named sibling and parent/child tests fail"
        [ expectToFail <|
            describe "a describe with two identically named children fails"
                [ test "foo" expectPass
                , test "foo" expectPass
                ]
        , expectToFail <|
            describe "a describe with the same name as a child test fails"
                [ test "a describe with the same name as a child test fails" expectPass
                ]
        , expectToFail <|
            describe "a describe with the same name as a child describe fails"
                [ describe "a describe with the same name as a child describe fails"
                    [ test "a test" expectPass ]
                ]
        , expectToFail <|
            Test.concat
                [ describe "a describe with the same name as a sibling describe fails"
                    [ test "a test" expectPass ]
                , describe "a describe with the same name as a sibling describe fails"
                    [ test "another test" expectPass ]
                ]
        , expectToFail <|
            Test.concat
                [ Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "a test" expectPass ]
                    ]
                , describe "a describe with the same name as a de facto sibling describe fails"
                    [ test "another test" expectPass ]
                ]
        , expectToFail <|
            Test.concat
                [ Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "a test" expectPass ]
                    ]
                , Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "another test" expectPass ]
                    ]
                ]
        ]
