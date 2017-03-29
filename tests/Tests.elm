module Tests exposing (all)

import Test exposing (..)
import Test.Expectation exposing (Expectation(..))
import Test.Runner
import Fuzz exposing (..)
import Dict
import Set
import Expect
import Helpers exposing (..)
import ExpectWithinTests exposing (testExpectWithin)
import FuzzerTests exposing (fuzzerTests)
import Test.Runner.Tests


all : Test
all =
    Test.concat
        [ readmeExample
        , regressions
        , testTests
        , expectationTests
        , fuzzerTests
        , Test.Runner.Tests.all
        ]


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
        , describe "Expect.all"
            [ expectToFail <|
                test "fails with empty list" <|
                    \_ -> "dummy subject" |> Expect.all []
            ]
        , testExpectWithin
        ]


regressions : Test
regressions =
    describe "regression tests"
        [ fuzz (intRange 1 32) "for #39" <|
            \positiveInt ->
                positiveInt
                    |> Expect.greaterThan 0
        ]


testTests : Test
testTests =
    describe "functions that create tests"
        [ describe "describe"
            [ expectToFail <| describe "fails with empty list" []
            , expectToFail <| describe "" [ test "describe with empty description fail" <| passingTest ]
            ]
        , describe "test"
            [ expectToFail <| test "" passingTest
            ]
        , describe "fuzz"
            [ expectToFail <| fuzz Fuzz.bool "" passingTest
            ]
        , describe "fuzzWith"
            [ expectToFail <| fuzzWith { runs = 0 } Fuzz.bool "nonpositive" passingTest
            , expectToFail <| fuzzWith { runs = 1 } Fuzz.bool "" passingTest
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
                [ test "foo" passingTest
                , test "foo" passingTest
                ]
        , expectToFail <|
            describe "a describe with the same name as a child test fails"
                [ test "a describe with the same name as a child test fails" passingTest
                ]
        , expectToFail <|
            describe "a describe with the same name as a child describe fails"
                [ describe "a describe with the same name as a child describe fails"
                    [ test "a test" passingTest ]
                ]
        , expectToFail <|
            Test.concat
                [ describe "a describe with the same name as a sibling describe fails"
                    [ test "a test" passingTest ]
                , describe "a describe with the same name as a sibling describe fails"
                    [ test "another test" passingTest ]
                ]
        , expectToFail <|
            Test.concat
                [ Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "a test" passingTest ]
                    ]
                , describe "a describe with the same name as a de facto sibling describe fails"
                    [ test "another test" passingTest ]
                ]
        , expectToFail <|
            Test.concat
                [ Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "a test" passingTest ]
                    ]
                , Test.concat
                    [ describe "a describe with the same name as a de facto sibling describe fails"
                        [ test "another test" passingTest ]
                    ]
                ]
        ]
