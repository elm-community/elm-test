module Test exposing (Test, describe, batch, test, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith)

{-| Testing

## Testing

@docs Test, test

## Grouping Tests

@docs describe, batch

## Fuzz Testing

@docs fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith
-}

import Random.Pcg as Random
import Test.Test
import Assert exposing (Assertion)
import Random.Pcg as Random exposing (Generator)
import Fuzz exposing (Fuzzer)


{-| A Test which has yet to be evaluated.

-}
type alias Test =
    Test.Test.Test


{-| Run all the given tests. (Execution order is not guaranteed.)

-- TODO give a code example.
-}
batch : List Test -> Test
batch =
    Test.Test.Batch


{-| Apply a description to a [`batch`](#batch) of tests.

-- TODO give a code example.
-}
describe : String -> List Test -> Test
describe desc =
    Test.Test.Batch >> Test.Test.Labeled desc


{-| Run a single `Test`.

-- TODO give a code example.
-}
test : String -> (() -> Assertion) -> Test
test desc thunk =
    Test.Test.Labeled desc (Test.Test.Test (\_ _ -> [ thunk () ]))


type alias FuzzOptions =
    { runs : Int }


{-| TODO document
-}
fuzzWith : FuzzOptions -> Fuzzer a -> String -> (a -> Assertion) -> Test
fuzzWith options fuzzer desc getTest =
    fuzzWithHelp options (Test.Test.fuzzTest desc fuzzer getTest)


fuzzWithHelp : FuzzOptions -> Test -> Test
fuzzWithHelp options test =
    case test of
        Test.Test.Test run ->
            Test.Test.Test (\seed _ -> run seed options.runs)

        Test.Test.Labeled label subTest ->
            Test.Test.Labeled label (fuzzWithHelp options subTest)

        Test.Test.Batch tests ->
            tests
                |> List.map (fuzzWithHelp options)
                |> Test.Test.Batch


{-| Run the given test several times, using a randomly-generated input from a
`Fuzzer` each time. By default, runs the test 100 times with different inputs,
but you can configure the run count using [`withRuns`](#withRuns).

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

-- TODO code sample
-}
fuzz :
    Fuzzer a
    -> String
    -> (a -> Assertion)
    -> Test
fuzz fuzzer desc =
    Test.Test.fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple`.


-- TODO code sample
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> String
    -> (a -> b -> Assertion)
    -> Test
fuzz2 fuzzA fuzzB desc =
    let
        fuzzer =
            Fuzz.tuple ( fuzzA, fuzzB )
    in
        uncurry >> Test.Test.fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple3`.

-- TODO code sample
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> String
    -> (a -> b -> c -> Assertion)
    -> Test
fuzz3 fuzzA fuzzB fuzzC desc =
    let
        fuzzer =
            Fuzz.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        uncurry3 >> Test.Test.fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple4`.

-- TODO code sample
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> String
    -> (a -> b -> c -> d -> Assertion)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD desc =
    let
        fuzzer =
            Fuzz.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        uncurry4 >> Test.Test.fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple5`.

-- TODO code sample
-}
fuzz5 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> (a -> b -> c -> d -> e -> Assertion)
    -> Test
fuzz5 desc fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        fuzzer =
            Fuzz.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        uncurry5 >> Test.Test.fuzzTest desc fuzzer



-- INTERNAL HELPERS --


defaults : { runs : Int, seed : Random.Seed }
defaults =
    { runs = 100
    , seed = Random.initialSeed 42
    }


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e
