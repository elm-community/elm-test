module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner as Runner
import Expect exposing (Expectation)
import Random.Pcg
import Snippets.FormattedText
import Snippets.Order
import Test.Internal exposing (Test(Labeled, Test))


main : Runner.BenchmarkProgram
main =
    Runner.program suite


suite : Benchmark
suite =
    describe "Fuzz"
        [ benchmark "Passing test using a fuzzed list" (benchTest Snippets.Order.test1)
        , benchmark "Failing test using a fuzzed list" (benchTest Snippets.Order.test2)
        , benchmark "Passing test using a fuzzer mapping" (benchTest Snippets.FormattedText.test1)
        , benchmark "Failing test using a fuzzer mapping" (benchTest Snippets.FormattedText.test2)
        ]


benchTest : Test -> (() -> List Expectation)
benchTest test =
    case test of
        Test fn ->
            \_ -> fn (Random.Pcg.initialSeed 0) 10

        Labeled _ test ->
            benchTest test

        test ->
            Debug.crash <| "No support for benchmarking this type of test: " ++ toString test
