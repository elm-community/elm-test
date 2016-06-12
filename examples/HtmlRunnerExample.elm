module HtmlRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-reactor

Visit http://localhost:8000 and bring up this file.
-}

import String
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


actualFuzzSuite : Suite
actualFuzzSuite =
    describe "actual fuzz suite"
        [ Test.singleton
            <| Test.fuzz usuallyFoo
            <| \shouldBeFoo ->
                { expected = "foo"
                , actual = shouldBeFoo
                }
                    |> Assert.equal
                    |> Assert.failWith "It wasn't \"foo\"."
        ]


main : Program Never
main =
    Test.Runner.Html.run suites


suites : Suite
suites =
    Batch
        [ oxfordifySuite
        , plainAssertion
        , assertionSuite
        , failFuzzSuite
        , actualFuzzSuite
        , fuzzSuite
        , shrinkableSuite
        ]


plainAssertion : Suite
plainAssertion =
    Test.singleton
        <| \_ ->
            { expected = "no description"
            , actual = "whatsoever!"
            }
                |> Assert.equal


assertionSuite : Suite
assertionSuite =
    describe "basic assertions"
        [ describe "this should succeed"
            [ Test.singleton
                <| \_ ->
                    { expected = ()
                    , actual = ()
                    }
                        |> Assert.equal
            ]
        , describe "this should fail"
            [ Test.singleton
                <| \_ ->
                    { expected = "something"
                    , actual = "someting else"
                    }
                        |> Assert.equal
            ]
        , Test.singleton
            <| \_ ->
                { expected = "forty-two"
                , actual = "forty-three"
                }
                    |> Assert.equal
        ]



--Html.text (toString <| runWithSeed (Random.initialSeed 42) actualFuzzSuite)
{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under Suite
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


fuzzSuite : Suite
fuzzSuite =
    describe "fuzz suite"
        [ Test.singleton
            <| (fuzz2 string string)
            <| \name punctuation ->
                { expected = ""
                , actual = oxfordify "This sentence is empty" "." []
                }
                    |> Assert.equal
                    |> Assert.failWith "given an empty list, did not return an empty string"
        , Test.singleton
            <| (fuzz2 string string)
            <| \name punctuation ->
                { expected = "This sentence contains one item."
                , actual = oxfordify "This sentence contains " "." [ "one item" ]
                }
                    |> Assert.equal
        , Test.singleton
            <| (fuzz2 string string)
            <| \name punctuation ->
                { expected = "This sentence contains one item and two item."
                , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
                }
                    |> Assert.equal
                    |> Assert.failWith "given an empty list, did not return an empty string"
        , Test.singleton
            <| (fuzz2 string string)
            <| \name punctuation ->
                { expected = "This sentence contains one item, two item, and three item."
                , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                }
                    |> Assert.equal
                    |> Assert.failWith "given a list of length 3, did not return an oxford-style sentence"
        ]


failFuzzSuite : Suite
failFuzzSuite =
    describe "the first element in this fuzz tuple"
        [ it "is always \"foo\""
            <| fuzz2 string string
            <| \str1 str2 ->
                Assert.equal
                    { expected = "foo"
                    , actual = str1
                    }
        ]


oxfordifySuite : Suite
oxfordifySuite =
    describe "oxfordify"
        [ describe "given an empty sentence"
            [ it "returns an empty string"
                <| \_ ->
                    Assert.equal
                        { expected = ""
                        , actual = oxfordify "This sentence is empty" "." []
                        }
            ]
        , describe "given a sentence with one item"
            [ it "still contains one item"
                <| \_ ->
                    Assert.equal
                        { expected = "This sentence contains one item."
                        , actual = oxfordify "This sentence contains " "." [ "one item" ]
                        }
            ]
        , describe "given a sentence with multiple items"
            [ it "returns an oxford-style sentence"
                <| \_ ->
                    Assert.equal
                        { expected = "This sentence contains one item and two item."
                        , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
                        }
            , it "returns an oxford-style sentence"
                <| \_ ->
                    Assert.equal
                        { expected = "This sentence contains one item, two item, and three item."
                        , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                        }
            ]
        ]


shrinkableSuite : Suite
shrinkableSuite =
    describe "Some Suites that should fail and produce shrunken values"
        [ describe "a randomly generated integer"
            [ it "is for sure exactly 0"
                <| (fuzz Fuzzer.int)
                <| \i ->
                    Assert.equal
                        { expected = 0
                        , actual = i
                        }
            , it "is <42"
                <| (fuzz Fuzzer.int)
                <| \i ->
                    Assert.lessThan
                        { greater = 42
                        , lesser = i
                        }
            , it "is also >42"
                <| (fuzz Fuzzer.int)
                <| \i ->
                    Assert.greaterThan
                        { greater = 42
                        , lesser = i
                        }
            ]
        , describe "a randomly generated string"
            [ it "equals its reverse"
                <| (fuzz Fuzzer.string)
                <| \s ->
                    Assert.equal
                        { expected = s
                        , actual = String.reverse s
                        }
            ]
        ]
