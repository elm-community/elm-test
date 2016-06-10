module HtmlRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-reactor

Visit http://localhost:8000 and bring up this file.
-}

import String
import Assert
import Test exposing (it, failWith)
import Suite exposing (..)
import Test.Runner.Html
import Fuzzer exposing (Fuzzer)
import Random.Pcg as Random
import Shrink


{-| TODO get rid of this once elm-format puts <| on the same line
-}
describe : String -> (a -> Suite) -> a -> Suite
describe str fn arg =
    Suite.describe str (fn arg)


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
        (Suite.fuzz usuallyFoo)
        [ \shouldBeFoo ->
            { expected = "foo"
            , actual = shouldBeFoo
            }
                |> Assert.equal
                |> failWith "It wasn't \"foo\"."
        ]


main : Program Never
main =
    Test.Runner.Html.run suites


suites : Suite
suites =
    Suite.batch
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
    Suite.unit
        [ \_ ->
            { expected = "no description"
            , actual = "whatsoever!"
            }
                |> Assert.equal
        ]


assertionSuite : Suite
assertionSuite =
    describe "basic assertions"
        Suite.batch
        [ describe "this should succeed"
            Suite.unit
            [ \_ ->
                { expected = ()
                , actual = ()
                }
                    |> Assert.equal
            ]
        , describe "this should fail"
            Suite.unit
            [ \_ ->
                { expected = "something"
                , actual = "someting else"
                }
                    |> Assert.equal
            ]
        , Suite.unit
            [ \_ ->
                { expected = "forty-two"
                , actual = "forty-three"
                }
                    |> Assert.equal
            ]
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
        (Suite.fuzz2 string string)
        [ \name punctuation ->
            { expected = ""
            , actual = oxfordify "This sentence is empty" "." []
            }
                |> Assert.equal
                |> failWith "given an empty list, did not return an empty string"
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
                |> failWith "given an empty list, did not return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item, two item, and three item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> Assert.equal
                |> failWith "given a list of length 3, did not return an oxford-style sentence"
        ]


failFuzzSuite : Suite
failFuzzSuite =
    describe "the first element in this fuzz tuple"
        (fuzz2 string string)
        [ \str1 str2 ->
            it "is always \"foo\""
                <| Assert.equal
                    { expected = "foo"
                    , actual = str1
                    }
        ]


oxfordifySuite : Suite
oxfordifySuite =
    describe "oxfordify"
        Suite.batch
        [ describe "given an empty sentence"
            Suite.unit
            [ \_ ->
                it "returns an empty string"
                    <| Assert.equal
                        { expected = ""
                        , actual = oxfordify "This sentence is empty" "." []
                        }
            ]
        , describe "given a sentence with one item"
            Suite.unit
            [ \_ ->
                it "still contains one item"
                    <| Assert.equal
                        { expected = "This sentence contains one item."
                        , actual = oxfordify "This sentence contains " "." [ "one item" ]
                        }
            ]
        , describe "given a sentence with multiple items"
            Suite.unit
            [ \_ ->
                it "returns an oxford-style sentence"
                    <| Assert.equal
                        { expected = "This sentence contains one item and two item."
                        , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
                        }
            , \_ ->
                it "returns an oxford-style sentence"
                    <| Assert.equal
                        { expected = "This sentence contains one item, two item, and three item."
                        , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                        }
            ]
        ]


shrinkableSuite : Suite
shrinkableSuite =
    describe "Some Suites that should fail and produce shrunken values"
        Suite.batch
        [ describe "Suites on one integer"
            (fuzz Fuzzer.int)
            [ \i ->
                it "Every integer is 0"
                    <| Assert.equal
                        { expected = 0
                        , actual = i
                        }
            , \i ->
                it "Every integer is <42"
                    <| Assert.lessThan
                        { greater = 42
                        , lesser = i
                        }
            , \i ->
                it "Every integer is >42"
                    <| Assert.greaterThan
                        { greater = 42
                        , lesser = i
                        }
            ]
        , describe "Suites on one string"
            (fuzz Fuzzer.string)
            [ \s ->
                it "Every string equals its reverse"
                    <| Assert.equal
                        { expected = s
                        , actual = String.reverse s
                        }
            ]
        ]
