module LogRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-make LogRunnerExample.elm --output=elm.js
$ node elm.js

Note that this always uses an initial seed of 42, since it can't do effects.
-}

import Assert
import Test exposing (..)
import Test.Runner.Log


main : Program Never
main =
    Test.Runner.Log.run suites


suites : Suite
suites =
    Test.batch [ oxfordifySuite ]


{-| stubbed function under Suite
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


oxfordifySuite : Suite
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
