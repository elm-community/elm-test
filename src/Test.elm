module Test exposing (Test, FuzzOptions, describe, batch, test, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith)

{-| ## Testing

@docs Test, test

## Grouping Tests

@docs describe, batch

## Fuzz Testing

@docs fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith, FuzzOptions
-}

import Random.Pcg as Random
import Test.Test
import Assert exposing (Assertion)
import Random.Pcg as Random exposing (Generator)
import Fuzz exposing (Fuzzer)


{-| A test which has yet to be evaluated. When evaluated, it produces one
or more [`Assertion`](../Assert#Assertion)s.

See [`test`](#test) and [`fuzz`](#fuzz) for some ways to create a `Test`.
-}
type alias Test =
    Test.Test.Test


{-| Run all the given tests. (Execution order is not guaranteed.)

    import Test exposing (batch)


    batch [ testDecoder, testSorting ]
-}
batch : List Test -> Test
batch =
    Test.Test.Batch


{-| Apply a description to a [`batch`](#batch) of tests.

    import Test exposing (describe, test, fuzz)
    import Fuzz expoing (int)
    import Assert


    describe "List"
        [ describe "reverse"
            [ test "has no effect on an empty list" <|
                \_ ->
                    Assert.equal
                        { expected = []
                        , actual = List.reverse []
                        }
            , fuzz int "has no effect on a one-item list" <|
                \num ->
                    Assert.equal
                        { expected = [ num ]
                        , actual = List.reverse [ num ]
                        }
            ]
        ]
-}
describe : String -> List Test -> Test
describe desc =
    Test.Test.Batch >> Test.Test.Labeled desc


{-| Return a [`Test`](#Test) that evaluates a single
[`Assertion`](../Assert#Assertion).

    import Test exposing (fuzz)
    import Assert


    test "the empty list has 0 length" <|
        \_ ->
            Assert.equal
                { expected = 0
                , actual = List.length []
                }
-}
test : String -> (() -> Assertion) -> Test
test desc thunk =
    Test.Test.Labeled desc (Test.Test.Test (\_ _ -> [ thunk () ]))


{-| Options [`fuzzWith`](#fuzzWith) accepts.

### `runs`

The number of times to run each fuzz test. (Default is 100.)

    import Test exposing (fuzzWith)
    import Fuzz exposing (list, int)
    import Assert


    fuzzWith { runs = 350 } (list int) "List.length should always be positive" <|
        -- This anonymous function will be run 350 times, each time with a
        -- randomly-generated fuzzList value. (It will always be a list of ints
        -- because of (list int) above.)
        \fuzzList ->
            Assert.atLeast 0 (List.length fuzzList)
-}
type alias FuzzOptions =
    { runs : Int }


{-| Run a [`fuzz`](#fuzz) test with the given [`FuzzOptions`](#FuzzOptions).

Note that there is no `fuzzWith2`, but you can always pass more fuzz values in
using [`Fuzz.tuple`](../Fuzz#tuple), [`Fuzz.tuple3`](../Fuzz#tuple3),
for example like this:

    import Test exposing (fuzzWith)
    import Fuzz exposing (tuple, list, int)
    import Assert


    fuzzWith { runs = 4200 }
        (tuple ( list int, int ))
        "List.reverse never influences List.member" <|
            \(nums, target) ->
                Assert.equal
                    { expected = List.member target nums
                    , actual = List.member target (List.reverse nums)
                    }
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

    import Test exposing (fuzz)
    import Fuzz exposing (list, int)
    import Assert


    fuzz (list int) "List.length should always be positive" <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value. (You can configure the run count
        -- using Fuzz.fuzzWith, or by giving your test runner a different default.)
        \fuzzList ->
            Assert.atLeast 0 (List.length fuzzList)
-}
fuzz :
    Fuzzer a
    -> String
    -> (a -> Assertion)
    -> Test
fuzz fuzzer desc =
    Test.Test.fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling [`Fuzz.tuple`](../Fuzz#tuple).

See [`fuzzWith`](#fuzzWith) for an example of writing this in tuple style.

    import Test exposing (fuzz2)
    import Fuzz exposing (list, int)


    fuzz2 (list int) int "List.reverse never influences List.member" <|
        \nums target ->
            Assert.equal
                { expected = List.member target nums
                , actual = List.member target (List.reverse nums)
                }
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

This is a convenicence function that lets you skip calling [`Fuzz.tuple3`](../Fuzz#tuple3).

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

This is a convenicence function that lets you skip calling [`Fuzz.tuple4`](../Fuzz#tuple4).
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

This is a convenicence function that lets you skip calling [`Fuzz.tuple5`](../Fuzz#tuple5).
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
