module Tests exposing (all)

import Test exposing (..)
import Test.Expectation exposing (Expectation(..))
import Test.Internal as TI
import Fuzz exposing (..)
import Dict
import Set
import String
import Expect
import Fuzz.Internal
import RoseTree
import Random.Pcg as Random


all : Test
all =
    Test.concat
        [ readmeExample, bug39, fuzzerTests, shrinkingTests ]


{-| Regression test for https://github.com/elm-community/elm-test/issues/39
-}
bug39 : Test
bug39 =
    fuzz (intRange 1 32) "small slice end" <|
        \positiveInt ->
            positiveInt
                |> Expect.greaterThan 0


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


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))


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
            [ fuzz (intRange 0 0xFFFFFFFF) "the same value is generated with and without shrinking" <|
                \i ->
                    let
                        seed =
                            Random.initialSeed i

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
            ]
        ]


testShrinking : Test -> Test
testShrinking test =
    case test of
        TI.Test runTest ->
            TI.Test
                (\seed runs ->
                    let
                        expectations =
                            runTest seed runs

                        goodShrink expectation =
                            case expectation of
                                Pass ->
                                    Just "Expected this test to fail, but it passed!"

                                Fail given outcome ->
                                    let
                                        acceptable =
                                            String.split "|" outcome
                                    in
                                        if List.member given acceptable then
                                            Nothing
                                        else
                                            Just <| "Got shrunken value " ++ given ++ " but expected " ++ String.join " or " acceptable
                    in
                        expectations
                            |> List.filterMap goodShrink
                            |> List.map Expect.fail
                            |> (\list ->
                                    if List.isEmpty list then
                                        [ Expect.pass ]
                                    else
                                        list
                               )
                )

        TI.Labeled desc labeledTest ->
            TI.Labeled desc (testShrinking labeledTest)

        TI.Batch tests ->
            TI.Batch (List.map testShrinking tests)


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
