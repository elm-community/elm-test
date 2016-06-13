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
fromTest : Random.Seed -> Int -> Test -> Runner
fromTest seed runs test =
    -- TODO independentSeed
    case test of
        Test.Test.Test run ->
            Thunk (\_ -> run seed runs)
                |> Runnable

        Test.Test.Labeled label subTest ->
            subTest
                |> fromTest seed runs
                |> Labeled label

        Test.Test.Batch tests ->
            tests
                |> List.map (fromTest seed runs)
                |> Batch


{-| TODO document
-}
type Runner
    = Runnable Runnable
    | Labeled String Runner
    | Batch (List Runner)
