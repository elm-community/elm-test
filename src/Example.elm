module Example exposing (..)

import Test exposing (..)


{-| stubbed function under test
-}
oxfordify _ _ _ =
  ""


fuzzSuite : Test
fuzzSuite =
  Test.fuzz2
    string
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
