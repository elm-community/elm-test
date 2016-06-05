module Example exposing (..)

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



--actualFuzzSuite : Test
--actualFuzzSuite =
--    Test.fuzz usuallyFoo
--        [ \shouldBeFoo ->
--            { expected = "foo"
--            , actual = shouldBeFoo
--            }
--                |> assertEqual
--                |> onFail "It wasn't \"foo\"."
--        ]


main : Program Never
main =
    Test.Runner.Html.run oxfordifySuite



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



--fuzzSuite : Test
--fuzzSuite =
--    Test.fuzz2 string
--        string
--        [ \name punctuation ->
--            { expected = ""
--            , actual = oxfordify "This sentence is empty" "." []
--            }
--                |> assertEqual
--                |> onFail "given an empty list, did not return an empty string"
--        , \name punctuation ->
--            { expected = "This sentence contains one item."
--            , actual = oxfordify "This sentence contains " "." [ "one item" ]
--            }
--                |> assertEqual
--        , \name punctuation ->
--            { expected = "This sentence contains one item and two item."
--            , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
--            }
--                |> assertEqual
--                |> onFail "given an empty list, did not return an empty string"
--        , \name punctuation ->
--            { expected = "This sentence contains one item, two item, and three item."
--            , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
--            }
--                |> assertEqual
--                |> onFail "given a list of length 3, did not return an oxford-style sentence"
--        ]


oxfordifySuite : Test
oxfordifySuite =
    Test.unit
        [ \_ ->
            { expected = ""
            , actual = oxfordify "This sentence is empty" "." []
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item."
            , actual = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> assertEqual
        , \_ ->
            { expected = "This sentence contains one item and two item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item, two item, and three item."
            , actual = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> assertEqual
                |> onFail "given a list of length 3, did not return an oxford-style sentence"
        ]
