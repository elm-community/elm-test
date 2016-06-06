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
    describe "Example tests"
        Test.batch
        [ actualFuzzSuite
        , oxfordifySuite
        , fuzzSuite
        , failingSuite
        ]


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


{-| Stubbed fuzzer
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


oxfordifySuite : Test
oxfordifySuite =
    Test.unit
        [ \_ ->
            { expected = ""
            , actual = oxfordify "This sentence is empty" "." []
            }
                |> Assert.equal
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item."
            , actual = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> Assert.equal
        , \_ ->
            { expected = "This sentence contains one item and two item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> Assert.equal
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item, two item, and three item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> Assert.equal
                |> onFail "given a list of length 3, did not return an oxford-style sentence"
        ]
