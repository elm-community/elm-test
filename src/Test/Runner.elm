module Test.Runner exposing (Runnable, Runner(..), run, fromTest)

{-| Running tests.

## Runner

@docs Runner, fromTest

## Runnable

@docs Runnable, run
-}

import Test exposing (Test)
import Test.Test
import Expect exposing (Expectation)
import Random.Pcg as Random


{-| An unevaluated test. Run it with [`run`](#run) to evaluate it into a
list of `Expectation`s.
-}
type Runnable
    = Thunk (() -> List Expectation)


{-| A structured test runner, incorporating:

* The expectations to run
* The hierarchy of description strings that describe the results
-}
type Runner
    = Runnable Runnable
    | Labeled String Runner
    | Batch (List Runner)


{-| Evaluate a [`Runnable`](#Runnable) to get a list of `Expectation`s.
-}
run : Runnable -> List Expectation
run (Thunk fn) =
    fn ()


{-| Convert a `Test` into a `Runner`. It requires a default run count as well
as an initial `Random.Seed` in order to run any fuzz tests that the `Test` may
have.

It's customary to use `100` as the initial run count and the current system time
to generate the initial seed.
-}
fromTest : Int -> Random.Seed -> Test -> Runner
fromTest runs seed test =
    case test of
        Test.Test.Test run ->
            Thunk (\() -> run seed runs)
                |> Runnable

        Test.Test.Labeled label subTest ->
            subTest
                |> fromTest runs seed
                |> Labeled label

        Test.Test.Batch subTests ->
            subTests
                |> List.foldl (distributeSeeds runs) ( seed, [] )
                |> snd
                |> Batch


distributeSeeds : Int -> Test -> ( Random.Seed, List Runner ) -> ( Random.Seed, List Runner )
distributeSeeds runs test ( startingSeed, runners ) =
    case test of
        Test.Test.Test run ->
            let
                ( seed, nextSeed ) =
                    Random.step Random.independentSeed startingSeed
            in
                ( nextSeed, runners ++ [ Runnable (Thunk (\() -> run seed runs)) ] )

        Test.Test.Labeled label subTest ->
            let
                ( nextSeed, nextRunners ) =
                    distributeSeeds runs subTest ( startingSeed, [] )

                finalRunners =
                    List.map (Labeled label) nextRunners
            in
                ( nextSeed, runners ++ finalRunners )

        Test.Test.Batch tests ->
            let
                ( nextSeed, nextRunners ) =
                    List.foldl (distributeSeeds runs) ( startingSeed, [] ) tests
            in
                ( nextSeed, [ Batch (runners ++ nextRunners) ] )
