module Test.Runner exposing (Runnable, Runner(..), run, fromTest)

{-| Running tests

@docs Runnable, Runner, run, fromTest
-}

import Test exposing (Test)
import Test.Test
import Assert exposing (Assertion)
import Random.Pcg as Random


{-| TODO document
-}
type Runnable
    = Thunk (() -> List Assertion)


{-| TODO document
-}
run : Runnable -> List Assertion
run (Thunk fn) =
    fn ()


{-| TODO document
-}
fromTest : Int -> Random.Seed -> Test -> Runner
fromTest runs seed test =
    case test of
        Test.Test.Test run ->
            Thunk (\_ -> run seed runs)
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
                ( nextSeed, runners ++ [ Runnable (Thunk (\_ -> run seed runs)) ] )

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


{-| TODO document
-}
type Runner
    = Runnable Runnable
    | Labeled String Runner
    | Batch (List Runner)
