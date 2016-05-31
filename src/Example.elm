module Example exposing (..)

import Test exposing (..)
import Random.Pcg as Random
import Html


{-| A fuzzzer that usually generates "foo", but occasonally "bar". We expect a claim that it's always "foo" to fail.
-}
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


actualFuzzSuite : Test
actualFuzzSuite =
    Test.fuzz usuallyFoo
        [ \shouldBeFoo ->
            { expected = "foo"
            , actually = shouldBeFoo
            }
                |> assertEqual
                |> onFail "It wasn't \"foo\"."
        ]


main =
    Html.text (toString <| runWithSeed (Random.initialSeed 42) actualFuzzSuite)



{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under test
-}
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


{-| Stubbed fuzzer
-}
string : Fuzzer String
string =
    Fuzzer
        <| Random.map
            (\b ->
                if b then
                    "foo"
                else
                    "bar"
            )
            Random.bool


fuzzSuite : Test
fuzzSuite =
    Test.fuzz2 string
        string
        [ \name punctuation ->
            { expected = ""
            , actually = oxfordify "This sentence is empty" "." []
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item."
            , actually = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> assertEqual
        , \name punctuation ->
            { expected = "This sentence contains one item and two item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item, two item, and three item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> assertEqual
                |> onFail "given a list of length 3, did not return an oxford-style sentence"
        ]
        |> onFail "oxfordify failed"


oxfordifySuite : Test
oxfordifySuite =
    Test.unit
        [ \_ ->
            { expected = ""
            , actually = oxfordify "This sentence is empty" "." []
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item."
            , actually = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> assertEqual
        , \_ ->
            { expected = "This sentence contains one item and two item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> assertEqual
                |> onFail "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item, two item, and three item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> assertEqual
                |> onFail "given a list of length 3, did not return an oxford-style sentence"
        ]
        |> onFail "oxfordify failed"
