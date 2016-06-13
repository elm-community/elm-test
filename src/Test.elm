module Test exposing (Test, Runnable(..), describe, toRunnable, batch, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| Testing

## Specifying Tests

@docs Test, describe, batch, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5

## Running Tests

@docs Runnable, toRunnable
-}

import Random.Pcg as Random
import Test.Assertion exposing (Assertion(..))
import Assert
import Dict
import Shrink
import Random.Pcg as Random exposing (Generator)
import Fuzz exposing (Fuzzer)


{-| TODO document
-}
type Runnable
    = Runnable (() -> Assertion)
    | Runnables (Maybe String) (List Runnable)


{-| A Test which has yet to be evaluated.

-}
type Test
    = Test (Random.Seed -> Int -> Assertion)
    | Labeled String Test
    | Batch (List Test)


{-| Run all the given tests. (Execution order is not guaranteed.)

-}
batch : List Test -> Test
batch =
    Batch


{-| TODO document
-}
toRunnable : Random.Seed -> Int -> Test -> Runnable
toRunnable seed runs test =
    case test of
        Test run ->
            Runnable (\_ -> run seed runs)

        Labeled label subSuite ->
            [ toRunnable seed runs subSuite ]
                |> Runnables (Just label)

        Batch suites ->
            suites
                |> List.map (toRunnable seed runs)
                |> Runnables Nothing


{-| Apply a description to a list of tests.

-- TODO give a code example.
-}
describe : String -> List Test -> Test
describe desc =
    Batch >> Labeled desc


{-| Run a single `Test`.

-- TODO give a code example.
-}
unit : String -> (() -> Assertion) -> Test
unit desc thunk =
    Labeled desc (Test (\_ _ -> thunk ()))


{-| Run the given tests several times, using a randomly-generated input from a
`Fuzzer` each time. By default, runs each test 100 times with different inputs,
but you can configure the run count using [`withRuns`](#withRuns).

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

-- TODO code sample
-}
fuzz :
    String
    -> Fuzzer a
    -> (a -> Assertion)
    -> Test
fuzz desc fuzzer =
    fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple`.


-- TODO code sample
-}
fuzz2 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> (a -> b -> Assertion)
    -> Test
fuzz2 desc fuzzA fuzzB =
    let
        fuzzer =
            Fuzz.tuple ( fuzzA, fuzzB )
    in
        uncurry >> fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple3`.

-- TODO code sample
-}
fuzz3 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> (a -> b -> c -> Assertion)
    -> Test
fuzz3 desc fuzzA fuzzB fuzzC =
    let
        fuzzer =
            Fuzz.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        uncurry3 >> fuzzTest desc fuzzer


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzz.tuple4`.

-- TODO code sample
-}
fuzz4 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> (a -> b -> c -> d -> Assertion)
    -> Test
fuzz4 desc fuzzA fuzzB fuzzC fuzzD =
    let
        fuzzer =
            Fuzz.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        uncurry4 >> fuzzTest desc fuzzer


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
        uncurry5 >> fuzzTest desc fuzzer



-- INTERNAL HELPERS --


defaults : { runs : Int, seed : Random.Seed }
defaults =
    { runs = 100
    , seed = Random.initialSeed 42
    }


fuzzTest : String -> Fuzzer a -> (a -> Assertion) -> Test
fuzzTest desc fuzzer getOutcome =
    let
        run seed runs =
            let
                runWithInput val =
                    let
                        outcome =
                            getOutcome val

                        shrunkenVal =
                            if outcome /= Assert.pass then
                                Shrink.shrink (getOutcome >> (/=) Assert.pass) fuzzer.shrinker val
                            else
                                val

                        shrunkenTest =
                            getOutcome shrunkenVal
                    in
                        ( Just (toString shrunkenVal), shrunkenTest )

                -- testRuns : Generator (List a)
                testRuns =
                    Random.list runs fuzzer.generator

                generators =
                    Random.map (List.map runWithInput) testRuns

                dedupe pairs =
                    pairs
                        |> List.map (\( mk, v ) -> ( Maybe.withDefault "" mk, v ))
                        |> Dict.fromList
                        |> Dict.toList
                        |> List.map
                            (\( s, v ) ->
                                ( if s == "" then
                                    Nothing
                                  else
                                    Just s
                                , v
                                )
                            )
            in
                seed
                    |> Random.step generators
                    |> fst
                    |> dedupe
                    |> List.map formatAssertion
                    |> concatAssertions
    in
        Labeled desc (Test run)


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e


formatAssertion : ( Maybe String, Assertion ) -> Assertion
formatAssertion ( input, outcome ) =
    formatFailure (prependInput input) outcome


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "Input: " ++ str ++ "\n\n" ++ original


{-| TODO document
-}
concatAssertions : List Assertion -> Assertion
concatAssertions =
    concatAssertionsHelp Pass


concatAssertionsHelp : Assertion -> List Assertion -> Assertion
concatAssertionsHelp result outcomes =
    case outcomes of
        [] ->
            result

        Pass :: rest ->
            case result of
                Pass ->
                    concatAssertionsHelp result rest

                (Fail _) as failure ->
                    concatAssertionsHelp failure rest

        ((Fail newFailures) as first) :: rest ->
            case result of
                Pass ->
                    concatAssertionsHelp first rest

                Fail oldFailures ->
                    concatAssertionsHelp (Fail (oldFailures ++ newFailures)) rest


formatFailure : (String -> String) -> Assertion -> Assertion
formatFailure format outcome =
    case outcome of
        Fail messages ->
            Fail (List.map format messages)

        Pass ->
            outcome
