module Test.Suite exposing (Suite, toRunners, batch, describe, withRuns, withSeed, withShrink, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| A collection of Tests.

@docs Suite, toRunners, batch, describe, withRuns, withSeed, withShrink, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5
-}

import Dict
import Shrink
import Random.Pcg as Random exposing (Generator)
import Test.Outcome exposing (Test, Outcome)
import Fuzzer exposing (Fuzzer)


{-| A batch of Tests which have yet to be evaluated. Execution order is not
guaranteed.

Use [`toRunners`](#toRunners) to convert a `Suite` into a list of
`() -> Test` functions, which can then be evaluated.
-}
type Suite
    = Tests (List String) (List Test)
    | Batch (List Suite)


{-| Turn a `Suite` into a list of thunks that can be run to produce Tests.

-- TODO code example
-}
toRunners : Random.Seed -> Int -> Suite -> List (() -> Outcome)
toRunners seed runs suite =
    case suite of
        Tests opts thunks ->
            List.concatMap (toTests seed opts) thunks

        Batch suites ->
            List.concatMap (toRunners seed) suites


{-| Override the default run count for fuzz tests in this suite. (By default,
[fuzz tests](#fuzz) run 100 times with randomly-generated inputs on each run.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withRuns : Int -> Suite -> Suite
withRuns runs =
    mapOptions (\opts -> { opts | runs = Maybe.oneOf [ opts.runs, Just runs ] })


{-| Override the default initial random number seed for fuzz tests in this suite.
(Most runners initialize [fuzz tests](#fuzz) with a seed based on the current
time when the runner starts up, but some use other approaches.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withSeed : Random.Seed -> Suite -> Suite
withSeed seed =
    mapOptions (\opts -> { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] })


{-| Override the default shrinking setting for fuzz tests in this suite.
(By default, [fuzz tests](#fuzz) have this set to `True`, meaning they will
perform shrinking when they fail. Shrinking searches for the minimal set of
inputs necessary to reproduce a failure, so you get a more concise error report
when a fuzz test fails. It's nice, but not instantaneous. This setting does not
matter when all tests pass, since shrinking happens only to failures.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withShrink : Bool -> Suite -> Suite
withShrink doShrink =
    mapOptions (\opts -> { opts | doShrink = Maybe.oneOf [ opts.doShrink, Just doShrink ] })


{-| Run a list of Suites.

See [`describe`](#describe) for running Suites with a descriptive string.

-- TODO give a code example.
-}
batch : List Suite -> Suite
batch =
    Batch


{-| Apply a description to a `Suite`.

-- TODO give a code example.
-}
describe : String -> Suite -> Suite
describe desc =
    mapOptions (prependFail desc)


{-| TODO: docs
-}
unit : List (() -> Test) -> Suite
unit fns =
    fns
        |> List.map (\fn -> (\_ -> [ fn () ]))
        |> Tests initialUnitOptions


initialUnitOptions : Options
initialUnitOptions =
    { onFail = []
    , runs = Just 1
    , doShrink = Just False
    , seed = Just defaults.seed
    }


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
    Fuzzer a
    -> List (a -> Test)
    -> Suite
fuzz fuzzer fuzzSuites =
    Tests initialOptions (List.map (fuzzToThunks fuzzer) fuzzSuites)


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple`.


-- TODO code sample
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> List (a -> b -> Test)
    -> Suite
fuzz2 fuzzA fuzzB =
    let
        fuzzer =
            Fuzzer.tuple ( fuzzA, fuzzB )
    in
        fuzzN (uncurry >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple3`.

-- TODO code sample
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> List (a -> b -> c -> Test)
    -> Suite
fuzz3 fuzzA fuzzB fuzzC =
    let
        fuzzer =
            Fuzzer.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        fuzzN (uncurry3 >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple4`.

-- TODO code sample
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> List (a -> b -> c -> d -> Test)
    -> Suite
fuzz4 fuzzA fuzzB fuzzC fuzzD =
    let
        fuzzer =
            Fuzzer.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        fuzzN (uncurry4 >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple5`.

-- TODO code sample
-}
fuzz5 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> List (a -> b -> c -> d -> e -> Test)
    -> Suite
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        fuzzer =
            Fuzzer.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        fuzzN (uncurry5 >> fuzzToThunks fuzzer)



-- INTERNAL HELPERS --


type alias Options =
    { onFail : List String
    , runs : Maybe Int
    , doShrink : Maybe Bool
    , seed : Maybe Random.Seed
    }


initialOptions : Options
initialOptions =
    { onFail = []
    , runs = Nothing
    , doShrink = Nothing
    , seed = Nothing
    }


defaults : { runs : Int, doShrink : Bool, seed : Random.Seed }
defaults =
    { runs = 100
    , doShrink = True
    , seed = Random.initialSeed 42
    }


toTests : List String -> Random.Seed -> Int -> (Random.Seed -> Int -> List Outcome) -> List (() -> Outcome)
toTests context seed runs getTests =
    getTests seed runs
        |> List.map (\test _ -> List.foldr Test.it test opts.onFail)


prependFail : String -> Options -> Options
prependFail str opts =
    { opts | onFail = str :: opts.onFail }


mapOptions : (Options -> Options) -> Suite -> Suite
mapOptions translate suite =
    case suite of
        Tests opts thunks ->
            Tests (translate opts) thunks

        Batch suites ->
            suites
                |> List.map (mapOptions translate)
                |> Batch


fuzzToThunks : Fuzzer a -> (a -> Test) -> Options -> List Test
fuzzToThunks fuzzer runAssert opts =
    let
        seed =
            Maybe.withDefault defaults.seed opts.seed

        runs =
            Maybe.withDefault defaults.runs opts.runs

        doShrink =
            Maybe.withDefault defaults.doShrink opts.doShrink

        runWithInput val =
            let
                test =
                    runAssert val

                shrunkenVal =
                    if doShrink && test /= Test.pass then
                        Shrink.shrink (runAssert >> (/=) Test.pass) fuzzer.shrinker val
                    else
                        val

                shrunkenTest =
                    if doShrink then
                        runAssert shrunkenVal
                    else
                        test
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
            |> List.map formatTest


fuzzN : (a -> Options -> List Test) -> List a -> Suite
fuzzN fn fuzzSuites =
    fuzzSuites
        |> List.map fn
        |> Tests initialOptions


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e


formatTest : ( Maybe String, Test ) -> Test
formatTest ( input, test ) =
    Test.formatFailures (prependInput input) test


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "Input: " ++ str ++ "\n\n" ++ original
