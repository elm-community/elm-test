module Tests exposing (all)

import Test exposing (..)
import Test.Expectation exposing (Expectation)
import Fuzz exposing (..)
import String
import Expect
import Fuzz.Internal
import RoseTree
import Random.Pcg as Random


all : Test
all =
    Test.concat
        [ readmeExample, bug39, fuzzerImpossible ]


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


fuzzerImpossible : Test
fuzzerImpossible =
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
                                , tuple4
                                    ( percentage
                                    , map2 (+) int int
                                    , frequencyOrCrash [ ( 1, constant True ), ( 3, constant False ) ]
                                    , Fuzz.filter (\i -> i % 10 /= 0) int
                                    )
                                , tuple3 ( intRange 0 100, floatRange -51 pi, map abs int )
                                )

                        valNoShrink =
                            aFuzzer |> Fuzz.Internal.unpackGenVal |> step |> fst

                        valWithShrink =
                            aFuzzer |> Fuzz.Internal.unpackGenTree |> step |> fst |> RoseTree.root
                    in
                        Expect.equal valNoShrink valWithShrink
            ]
        ]
