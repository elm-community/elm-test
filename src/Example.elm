module Example exposing (..)

import Assert
import Test exposing (..)
import Test.Runner.Html
import Fuzzer exposing (Fuzzer)
import Random.Pcg as Random
import Shrink


{-| A fuzzzer that usually generates "foo", but occasonally "bar". We expect a claim that it's always "foo" to fail.
-}
usuallyFoo : Fuzzer String
usuallyFoo =
    Fuzzer
        (Random.oneIn 30
            |> Random.map
                (\b ->
                    if b then
                        "bar"
                    else
                        "foo"
                )
        )
        Shrink.string


actualFuzzSuite : Test
actualFuzzSuite =
    describe "actual fuzz suite"
        (Test.fuzz usuallyFoo)
        [ \shouldBeFoo ->
            { expected = "foo"
            , actual = shouldBeFoo
            }
                |> Assert.equal
                |> onFail "It wasn't \"foo\"."
        ]


main : Program Never
main =
    Test.Runner.Html.run tests


tests : Test
tests =
    Test.batch
        [ oxfordifySuite
        , splineSuite
        , actualFuzzSuite
        , fuzzSuite
        , fuzzSuite
        , failingSuite
        ]



-- TODO FIXME if you change or remove this `runs` call, the output changes a LOT!
--|> runs 10000


failingSuite : Test
failingSuite =
    describe "failing suite"
        Test.unit
        [ \_ ->
            { expected = "something"
            , actual = "someting else"
            }
                |> Assert.equal
        ]



--Html.text (toString <| runWithSeed (Random.initialSeed 42) actualFuzzSuite)
{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under test
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


{-| Stubbed fuzzer - TODO implement
-}
string : Fuzzer String
string =
    Fuzzer (Random.choice "foo" "bar")
        Shrink.string


fuzzSuite : Test
fuzzSuite =
    describe "fuzz suite"
        (Test.fuzz2 string string)
        [ \name punctuation ->
            { expected = ""
            , actual = oxfordify "This sentence is empty" "." []
            }
                |> Assert.equal
                |> onFail "given an empty list, did not return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item."
            , actual = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> Assert.equal
        , \name punctuation ->
            { expected = "This sentence contains one item and two item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> Assert.equal
                |> onFail "given an empty list, did not return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item, two item, and three item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> Assert.equal
                |> onFail "given a list of length 3, did not return an oxford-style sentence"
        ]


splineSuite : Test
splineSuite =
    describe "spline reticulator"
        (fuzz2 string string)
        [ \str1 str2 ->
            it "properly reticulates splines"
                Assert.equal
                { expected = str1 ++ "blah"
                , actual = str2
                }
        ]


oxfordifySuite : Test
oxfordifySuite =
    describe "oxfordify"
        Test.batch
        [ describe "given an empty sentence"
            Test.unit
            [ \_ ->
                it "returns an empty string"
                    Assert.equal
                    { expected = ""
                    , actual = oxfordify "This sentence is empty" "." []
                    }
            ]
        , describe "given a sentence with one item"
            Test.unit
            [ \_ ->
                it "still contains one item"
                    Assert.equal
                    { expected = "This sentence contains one item."
                    , actual = oxfordify "This sentence contains " "." [ "one item" ]
                    }
            ]
        , describe "given a sentence with multiple items"
            Test.unit
            [ \_ ->
                it "returns an oxford-style sentence"
                    Assert.equal
                    { expected = "This sentence contains one item and two item."
                    , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
                    }
            , \_ ->
                it "returns an oxford-style sentence"
                    Assert.equal
                    { expected = "This sentence contains one item, two item, and three item."
                    , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                    }
            ]
        ]
