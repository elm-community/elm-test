module Test.Runner exposing (Runnable, Runner(..), run, fromTest, formatLabels)

{-| A collection of functions used by authors of test runners. To run your
own tests, you should use these runners; see the `README` for more information.

## Runner

@docs Runner, fromTest

## Runnable

@docs Runnable, run

## Formatting

@docs formatLabels
-}

import Test exposing (Test)
import Test.Internal as Internal
import Expect exposing (Expectation)
import Random.Pcg
import String


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


{-| Convert a `Test` into a `Runner`.

In order to run any fuzz tests that the `Test` may have, it requires a default run count as well
as an initial `Random.Pcg.Seed`. `100` is a good run count. To obtain a good random seed, pass a
random 32-bit integer to `Random.Pcg.initialSeed`. You can obtain such an integer by running
`Math.floor(Math.random()*0xFFFFFFFF)` in Node. It's typically fine to hard-code this value into
your Elm code; it's easy and makes your tests reproducible.
-}
fromTest : Int -> Random.Pcg.Seed -> Test -> Runner
fromTest runs seed test =
    if runs < 1 then
        Thunk (\() -> [ Expect.fail ("Test runner run count must be at least 1, not " ++ toString runs) ])
            |> Runnable
    else
        case test of
            Internal.Test run ->
                Thunk (\() -> run seed runs)
                    |> Runnable

            Internal.Labeled label subTest ->
                subTest
                    |> fromTest runs seed
                    |> Labeled label

            Internal.Batch subTests ->
                subTests
                    |> List.foldl (distributeSeeds runs) ( seed, [] )
                    |> Tuple.second
                    |> Batch


distributeSeeds : Int -> Test -> ( Random.Pcg.Seed, List Runner ) -> ( Random.Pcg.Seed, List Runner )
distributeSeeds runs test ( startingSeed, runners ) =
    case test of
        Internal.Test run ->
            let
                ( seed, nextSeed ) =
                    Random.Pcg.step Random.Pcg.independentSeed startingSeed
            in
                ( nextSeed, runners ++ [ Runnable (Thunk (\() -> run seed runs)) ] )

        Internal.Labeled label subTest ->
            let
                ( nextSeed, nextRunners ) =
                    distributeSeeds runs subTest ( startingSeed, [] )

                finalRunners =
                    List.map (Labeled label) nextRunners
            in
                ( nextSeed, runners ++ finalRunners )

        Internal.Batch tests ->
            let
                ( nextSeed, nextRunners ) =
                    List.foldl (distributeSeeds runs) ( startingSeed, [] ) tests
            in
                ( nextSeed, [ Batch (runners ++ nextRunners) ] )


{-| A standard way to format descriptiona and test labels, to keep things
consistent across test runner implementations.

The HTML, Node, String, and Log runners all use this.

What it does:

* drop any labels that are empty strings
* format the first label differently from the others
* reverse the resulting list

    [ "the actual test that failed"
    , "nested description failure"
    , "top-level description failure"
    ]
        |> formatLabels ((++) "↓ ") ((++) "✗ ")

    {-
        [ "↓ top-level description failure"
        , "↓ nested description failure"
        , "✗ the actual test that failed"
        ]
    -}

-}
formatLabels :
    (String -> format)
    -> (String -> format)
    -> List String
    -> List format
formatLabels formatDescription formatTest labels =
    case List.filter (not << String.isEmpty) labels of
        [] ->
            []

        test :: descriptions ->
            descriptions
                |> List.map formatDescription
                |> (::) (formatTest test)
                |> List.reverse
