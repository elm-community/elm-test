module Test exposing (Test, FuzzOptions, describe, test, filter, concat, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith)

{-| A module containing functions for creating and managing tests.

@docs Test, test

## Organizing Tests

@docs describe, concat, filter

## Fuzz Testing

@docs fuzz, fuzz2, fuzz3, fuzz4, fuzz5, fuzzWith, FuzzOptions
-}

import Test.Internal as Internal
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)


{-| A test which has yet to be evaluated. When evaluated, it produces one
or more [`Expectation`](../Expect#Expectation)s.

See [`test`](#test) and [`fuzz`](#fuzz) for some ways to create a `Test`.
-}
type alias Test =
    Internal.Test


{-| Run each of the given tests.

    concat [ testDecoder, testSorting ]
-}
concat : List Test -> Test
concat =
    Internal.Batch


{-| Remove any test unless its description satisfies the given predicate
function. Nested descriptions added with [`describe`](#describe) are not considered.

    describe "String.reverse"
        [ test "has no effect on a palindrome" testGoesHere
        , test "reverses a known string" anotherTest
        , fuzz string "restores the original string if you run it again" oneMore
        ]
            |> Test.filter (String.contains "original")

    -- only runs the final test

You can use this to focus on a specific test or two, silencing the failures of
tests you don't want to work on yet, and then remove the call to `Test.filter`
after you're done working on the tests.
-}
filter : (String -> Bool) -> Test -> Test
filter =
    Internal.filter


{-| Apply a description to a list of tests.

    import Test exposing (describe, test, fuzz)
    import Fuzz exposing (int)
    import Expect


    describe "List"
        [ describe "reverse"
            [ test "has no effect on an empty list" <|
                \() ->
                    List.reverse []
                        |> Expect.toEqual []
            , fuzz int "has no effect on a one-item list" <|
                \num ->
                     List.reverse [ num ]
                        |> Expect.toEqual [ num ]
            ]
        ]
-}
describe : String -> List Test -> Test
describe desc =
    Internal.Batch >> Internal.Labeled desc


{-| Return a [`Test`](#Test) that evaluates a single
[`Expectation`](../Expect#Expectation).

    import Test exposing (fuzz)
    import Expect


    test "the empty list has 0 length" <|
        \() ->
            List.length []
                |> Expect.toEqual 0
-}
test : String -> (() -> Expectation) -> Test
test desc thunk =
    Internal.Labeled desc (Internal.Test (\_ _ -> [ thunk () ]))


{-| Options [`fuzzWith`](#fuzzWith) accepts. Currently there is only one but this
API is designed so that it can accept more in the future.

### `runs`

The number of times to run each fuzz test. (Default is 100.)

    import Test exposing (fuzzWith)
    import Fuzz exposing (list, int)
    import Expect


    fuzzWith { runs = 350 } (list int) "List.length should always be positive" <|
        -- This anonymous function will be run 350 times, each time with a
        -- randomly-generated fuzzList value. (It will always be a list of ints
        -- because of (list int) above.)
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0
-}
type alias FuzzOptions =
    { runs : Int }


{-| Run a [`fuzz`](#fuzz) test with the given [`FuzzOptions`](#FuzzOptions).

Note that there is no `fuzzWith2`, but you can always pass more fuzz values in
using [`Fuzz.tuple`](../Fuzz#tuple), [`Fuzz.tuple3`](../Fuzz#tuple3),
for example like this:

    import Test exposing (fuzzWith)
    import Fuzz exposing (tuple, list, int)
    import Expect


    fuzzWith { runs = 4200 }
        (tuple ( list int, int ))
        "List.reverse never influences List.member" <|
            \(nums, target) ->
                List.member target (List.reverse nums)
                    |> Expect.toEqual (List.member target nums)
-}
fuzzWith : FuzzOptions -> Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzWith options fuzzer desc getTest =
    if options.runs < 1 then
        test desc <|
            \() ->
                Expect.fail ("Fuzz test run count must be at least 1, not " ++ toString options.runs)
    else
        fuzzWithHelp options (fuzz fuzzer desc getTest)


fuzzWithHelp : FuzzOptions -> Test -> Test
fuzzWithHelp options test =
    case test of
        Internal.Test run ->
            Internal.Test (\seed _ -> run seed options.runs)

        Internal.Labeled label subTest ->
            Internal.Labeled label (fuzzWithHelp options subTest)

        Internal.Batch tests ->
            tests
                |> List.map (fuzzWithHelp options)
                |> Internal.Batch


{-| Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Fuzz) each time. This allows you to
test that a property that should always be true is indeed true under a wide variety of conditions. The function also
takes a string describing the test.

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

    import Test exposing (fuzz)
    import Fuzz exposing (list, int)
    import Expect


    fuzz (list int) "List.length should always be positive" <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value.
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0
-}
fuzz :
    Fuzzer a
    -> String
    -> (a -> Expectation)
    -> Test
fuzz =
    Internal.fuzzTest


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenience function that lets you skip calling [`Fuzz.tuple`](../Fuzz#tuple).

See [`fuzzWith`](#fuzzWith) for an example of writing this in tuple style.

    import Test exposing (fuzz2)
    import Fuzz exposing (list, int)


    fuzz2 (list int) int "List.reverse never influences List.member" <|
        \nums target ->
            List.member target (List.reverse nums)
                |> Expect.toEqual (List.member target nums)
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> String
    -> (a -> b -> Expectation)
    -> Test
fuzz2 fuzzA fuzzB desc =
    let
        fuzzer =
            Fuzz.tuple ( fuzzA, fuzzB )
    in
        uncurry >> fuzz fuzzer desc


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenience function that lets you skip calling [`Fuzz.tuple3`](../Fuzz#tuple3).
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> String
    -> (a -> b -> c -> Expectation)
    -> Test
fuzz3 fuzzA fuzzB fuzzC desc =
    let
        fuzzer =
            Fuzz.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        uncurry3 >> fuzz fuzzer desc


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenience function that lets you skip calling [`Fuzz.tuple4`](../Fuzz#tuple4).
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> String
    -> (a -> b -> c -> d -> Expectation)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD desc =
    let
        fuzzer =
            Fuzz.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        uncurry4 >> fuzz fuzzer desc


{-| Run a [fuzz test](#fuzz) using five random inputs.

This is a convenience function that lets you skip calling [`Fuzz.tuple5`](../Fuzz#tuple5).
-}
fuzz5 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> String
    -> (a -> b -> c -> d -> e -> Expectation)
    -> Test
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE desc =
    let
        fuzzer =
            Fuzz.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        uncurry5 >> fuzz fuzzer desc



-- INTERNAL HELPERS --


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e
