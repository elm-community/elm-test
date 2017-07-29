module SeedTests exposing (fixedSeed, noAutoFail, tests)

import Bitwise
import Expect exposing (FloatingPointTolerance(Absolute, AbsoluteOrRelative, Relative))
import Fuzz exposing (..)
import Random.Pcg as Random
import Test exposing (..)


-- NOTE: These tests are only here so that we can watch out for regressions. All constants in this file are what the implementation happened to output, not what we expected the implementation to output.
--  "-3954212174" "340755122"


expectedNum : Int
expectedNum =
    fx -3954212174


oneSeedAlreadyDistributed : Int
oneSeedAlreadyDistributed =
    fx -18


fixedSeed : Random.Seed
fixedSeed =
    Random.initialSeed 133742


fx =
    Bitwise.shiftRightZfBy 0


{-| Most of the tests will use this, but we won't run it directly.

When these tests are run using fixedSeed and a run count of 1, this is the
exact number they will get when the description around this fuzz test is
exactly the string "Seed test".

-}
fuzzTest : Test
fuzzTest =
    fuzz int "It receives the expected number" <|
        \num ->
            expectedNum |> Expect.equal (fx num)


fuzzTestAfterOneDistributed : Test
fuzzTestAfterOneDistributed =
    fuzz int "This should be different than expectedNum, because there is a fuzz test before it." <|
        \num ->
            oneSeedAlreadyDistributed |> Expect.equal (fx num) |> Debug.log "potato"


tests : List Test
tests =
    [ describe "Seed test"
        [ fuzzTest ]
    , describe "Seed test"
        [ fuzz int "It receives the expected number even though this text is different" <|
            \num ->
                expectedNum |> Expect.equal (fx num)
        ]
    , describe "Seed test"
        [ describe "Nested describes shouldn't affect seed distribution"
            [ fuzzTest ]
        ]
    , describe "Seed test"
        [ test "Unit tests before should not affect seed distribution" <|
            \_ ->
                Expect.pass
        , fuzzTest
        , test "Unit tests after should not affect seed distribution" <|
            \_ ->
                Expect.pass
        ]
    , -- Wrapping in a Test.concat shouldn't change anything
      Test.concat
        [ describe "Seed test"
            [ fuzzTest ]
        ]
    , -- Wrapping in a Test.concat wth unit tests shouldn't change anything
      Test.concat
        [ describe "Seed test"
            [ test "Unit tests before should not affect seed distribution" <|
                \_ ->
                    Expect.pass
            , fuzzTest
            , test "Unit tests after should not affect seed distribution" <|
                \_ ->
                    Expect.pass
            ]
        ]
    , -- Putting a fuzz test before it, within a second label, *should* change things
      Test.concat
        [ describe "Seed test"
            [ fuzzTest
            , fuzzTestAfterOneDistributed
            ]
        ]
    , Test.concat
        [ fuzz int "top-level fuzz tests don't affect subsequent top-level fuzz tests, since they use their labels to get different seeds" <|
            \num ->
                409469537 |> Expect.equal (fx num)
        , describe "Seed test"
            [ fuzzTest ]
        , describe "another top-level fuzz test"
            [ fuzz int "it still gets different values, due to computing the seed as a hash of the label, and these labels must be unique" <|
                \num ->
                    0 |> Expect.equal (fx num)
            ]
        ]
    , describe "Fuzz tests with different outer describe texts get different seeds"
        [ fuzz int "It receives the expected number" <|
            \num ->
                2049737128 |> Expect.equal (fx num)
        ]
    ]


noAutoFail : List Test
noAutoFail =
    [ -- Test.skip does not affect seed distribution
      Test.concat
        [ describe "Seed test"
            [ skip fuzzTest
            , fuzzTestAfterOneDistributed
            ]
        ]
    , -- Test.only does not affect seed distribution
      Test.concat
        [ describe "Seed test"
            [ only fuzzTest ]
        ]
    , -- Test.only skips the other tests in question
      Test.concat
        [ describe "Seed test"
            [ skip <|
                test "Autofail" <|
                    \_ ->
                        Expect.fail "Test.skip is broken! This should not have been run."
            , fuzzTest
            ]
        ]
    , -- Test.only skips the other tests.
      Test.concat
        [ describe "Seed test"
            [ only <|
                fuzz int "No Autofail here" <|
                    \num ->
                        expectedNum |> Expect.equal (fx num)
            , test "This should never get run" <|
                \() ->
                    Expect.fail "Test.only is broken! This should not have been run."
            ]
        ]
    , -- Test.skip skips the test in question
      describe "Seed test"
        [ skip <|
            fuzz int "Skip test sanity check" <|
                \_ ->
                    Expect.fail "Test.skip is broken! This should not have been run."
        , fuzzTestAfterOneDistributed
        ]
    , -- the previous test gets the same answer if Test.skip is removed
      describe "Seed test"
        [ fuzz int "Skip test sanity check" <|
            \_ ->
                Expect.pass
        , fuzzTestAfterOneDistributed
        ]
    , -- Test.only skips the other tests.
      describe "Seed test"
        [ only <|
            fuzz int "No Autofail here" <|
                \num ->
                    expectedNum |> Expect.equal (fx num)
        , test "this should never get run" <|
            \() ->
                Expect.fail "Test.only is broken! This should not have been run."
        ]
    , -- Test.only does not affect seed distribution
      describe "Seed test"
        [ test "Autofail" <|
            \_ -> Expect.fail "Test.only is broken! This should not have been run."
        , fuzzTest
        , only <|
            fuzzTestAfterOneDistributed
        ]
    , -- the previous test gets the same answer if Test.only is removed
      describe "Seed test"
        [ test "Autofail" <|
            \_ -> Expect.pass
        , fuzzTest
        , fuzzTestAfterOneDistributed
        ]
    ]
