module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner as Runner
import Expect
import Fuzz
import Random.Pcg
import Test.Fuzz exposing (getFailures)


main : Runner.BenchmarkProgram
main =
    Runner.program suite


suite : Benchmark
suite =
    let
        mapFuzzer =
            Fuzz.int
                |> Fuzz.map ((+) 1)
                |> Fuzz.map ((+) 1)
                |> Fuzz.map ((+) 1)
                |> Fuzz.map ((+) 1)

        andMapFuzzer =
            Fuzz.constant (,,,)
                |> Fuzz.andMap Fuzz.int
                |> Fuzz.andMap Fuzz.int
                |> Fuzz.andMap Fuzz.int
                |> Fuzz.andMap Fuzz.int

        andThenFuzzer =
            Fuzz.bool
                |> Fuzz.andThen (\_ -> Fuzz.bool)
                |> Fuzz.andThen (\_ -> Fuzz.bool)
                |> Fuzz.andThen (\_ -> Fuzz.bool)
                |> Fuzz.andThen (\_ -> Fuzz.bool)

        seed =
            Random.Pcg.initialSeed 0

        pass _ =
            Expect.pass

        fail _ =
            Expect.fail "Oops"
    in
    describe "Fuzz"
        [ benchmark4 "map - generating" getFailures mapFuzzer pass seed 100
        , benchmark4 "map - shrinking" getFailures mapFuzzer fail seed 100
        , benchmark4 "andMap - generating" getFailures andMapFuzzer pass seed 100
        , benchmark4 "andMap - shrinking" getFailures andMapFuzzer fail seed 100
        , benchmark4 "andThen - generating" getFailures andThenFuzzer pass seed 100
        , benchmark4 "andThen - shrinking" getFailures andThenFuzzer fail seed 100
        ]
