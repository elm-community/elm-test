module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner as Runner
import Expect exposing (Expectation)
import Random.Pcg
import Snippets
import Test.Internal exposing (Test(Labeled, Test))


main : Runner.BenchmarkProgram
main =
    Runner.program suite


suite : Benchmark
suite =
    describe "Fuzz"
        [ describe "list of int"
            [ benchmark "generating" (benchTest Snippets.listIntPass)
            , benchmark "shrinking" (benchTest Snippets.listIntFail)
            ]
        , describe "andMap"
            [ benchmark "generating" (benchTest Snippets.andMapPass)
            , benchmark "shrinking" (benchTest Snippets.andMapFail)
            ]
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
