module SeedTests exposing (fixedSeed, tests)

import Expect exposing (FloatingPointTolerance(Absolute, AbsoluteOrRelative, Relative))
import Fuzz exposing (..)
import Random.Pcg as Random
import Test exposing (..)


fixedSeed : Random.Seed
fixedSeed =
    Random.initialSeed 133742


expectedNum : Int
expectedNum =
    1083264770


{-| Most of the tests will use this, but we won't run it directly.

When these tests are run using fixedSeed and a run count of 1, this is the
exact number they will get when the description around this fuzz test is
exactly the string "Seed test".

-}
fuzzTest : Test
fuzzTest =
    fuzz int "It receives the expected number" <|
        \num ->
            Expect.equal num expectedNum


tests : List Test
tests =
    [ describe "Seed test"
        [ fuzzTest ]
    , describe "Seed test"
        [ fuzz int "It receives the expected number even though this text is different" <|
            \num ->
                Expect.equal num expectedNum
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
    ]
