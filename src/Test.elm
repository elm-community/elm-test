module Test exposing (Test, toRunners, batch, describe, it, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, onFail, withRuns, withSeed, withShrink)

{-|

@docs Test, batch, describe, it, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, toRunners, onFail, withRuns, withSeed, withShrink
-}

import Fuzzer exposing (Fuzzer)
import Assert exposing (Outcome)
import Random.Pcg as Random exposing (Generator)


-- none of the types below will be exported, except Test which will be opaque


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
    , doShrink = False
    , seed = Random.initialSeed 42
    }


{-| A Test is either
   * A list of thunks, each of which returns an Outcome
   * A batch of these tests
-}
type Test
    = Assertions Options (List (Options -> Outcome))
    | Batch (List Test)


{-| Turn a `Test` into a list of thunks that can be run to produce outcomes.
-}
toRunners : Random.Seed -> Test -> List (() -> Outcome)
toRunners seed test =
    case test of
        Assertions opts thunks ->
            List.map (toOutcome seed opts) thunks

        Batch tests ->
            List.concatMap (toRunners seed) tests


toOutcome : Random.Seed -> Options -> (Options -> Outcome) -> () -> Outcome
toOutcome seed opts getAssertion _ =
    let
        outcome =
            getAssertion { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] }
    in
        List.foldr Assert.addContext outcome opts.onFail


{-| TODO: docs
-}
onFail : String -> Outcome -> Outcome
onFail str outcome =
    Assert.formatFailures (\_ -> str) outcome


{-| TODO: docs
-}
it : String -> (a -> Outcome) -> a -> Outcome
it str getOutcome arg =
    Assert.addContext str (getOutcome arg)


{-| TODO: docs
-}
withRuns : Int -> Test -> Test
withRuns runs =
    mapOptions (\opts -> { opts | runs = Maybe.oneOf [ opts.runs, Just runs ] })


{-| TODO: docs
-}
withSeed : Random.Seed -> Test -> Test
withSeed seed =
    mapOptions (\opts -> { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] })


{-| TODO: docs
-}
withShrink : Bool -> Test -> Test
withShrink doShrink =
    mapOptions (\opts -> { opts | doShrink = Maybe.oneOf [ opts.doShrink, Just doShrink ] })


mapOptions : (Options -> Options) -> Test -> Test
mapOptions translate test =
    case test of
        Assertions opts thunks ->
            Assertions (translate opts) thunks

        Batch tests ->
            tests
                |> List.map (mapOptions translate)
                |> Batch


{-| TODO: docs
-}
unit : List (() -> Outcome) -> Test
unit fns =
    fns
        |> List.map (\fn -> (\_ -> fn ()))
        |> Assertions initialUnitOptions


initialUnitOptions : Options
initialUnitOptions =
    { onFail = []
    , runs = Just 1
    , doShrink = Just False
    , seed = Just defaults.seed
    }


{-| TODO docs
-}
fuzz :
    Fuzzer a
    -> List (a -> Outcome)
    -> Test
fuzz fuzzer fuzzTests =
    Assertions initialOptions (List.map (fuzzToThunk fuzzer) fuzzTests)


{-| TODO docs
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> List (a -> b -> Outcome)
    -> Test
fuzz2 fuzzA fuzzB =
    let
        fuzzer =
            Fuzzer.tuple ( fuzzA, fuzzB )
    in
        fuzzN (uncurry >> fuzzToThunk fuzzer)


{-| TODO docs
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> List (a -> b -> c -> Outcome)
    -> Test
fuzz3 fuzzA fuzzB fuzzC =
    let
        fuzzer =
            Fuzzer.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        fuzzN (uncurry3 >> fuzzToThunk fuzzer)


{-| TODO docs
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> List (a -> b -> c -> d -> Outcome)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD =
    let
        fuzzer =
            Fuzzer.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        fuzzN (uncurry4 >> fuzzToThunk fuzzer)


{-| TODO docs
-}
fuzz5 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> List (a -> b -> c -> d -> e -> Outcome)
    -> Test
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        fuzzer =
            Fuzzer.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        fuzzN (uncurry5 >> fuzzToThunk fuzzer)


{-| Run a list of tests.

See [`describe`](#describe) for running tests with a descriptive string.
-}
batch : List Test -> Test
batch =
    Batch


{-| Run a test and associate the given description.
-}
describe : String -> (a -> Test) -> a -> Test
describe desc getTest arg =
    mapOptions (prependFail desc) (getTest arg)


prependFail : String -> Options -> Options
prependFail str opts =
    { opts | onFail = str :: opts.onFail }


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e


fuzzToThunk : Fuzzer a -> (a -> Outcome) -> Options -> Outcome
fuzzToThunk fuzzer runAssert opts =
    let
        seed =
            Maybe.withDefault defaults.seed opts.seed

        runs =
            Maybe.withDefault defaults.runs opts.runs

        doShrink =
            Maybe.withDefault defaults.doShrink opts.doShrink

        runWithInput val =
            ( Just (toString val), runAssert val )

        -- testRuns : Generator (List a)
        testRuns =
            Random.list runs fuzzer.generator
    in
        if doShrink then
            let
                generators =
                    -- TODO instead using runAssert alone, do some function
                    -- that also returns whatever info shrinking needs later
                    -- in order to only attempt to shrink failures.
                    Random.map (List.map runAssert) testRuns

                ( preliminaryResults, _ ) =
                    Random.step generators seed
            in
                if List.all Assert.isSuccess preliminaryResults then
                    Assert.succeed
                else
                    -- TODO do shrinking here instead of this logic!
                    seed
                        |> Random.step (Random.map (List.map runWithInput) testRuns)
                        |> fst
                        |> List.map formatOutcome
                        |> Assert.concatOutcomes
        else
            let
                generators =
                    Random.map (List.map runWithInput) testRuns
            in
                seed
                    |> Random.step generators
                    |> fst
                    |> List.map formatOutcome
                    |> Assert.concatOutcomes


formatOutcome : ( Maybe String, Outcome ) -> Outcome
formatOutcome ( input, outcome ) =
    Assert.formatFailures (prependInput input) outcome


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "Input: " ++ str ++ "\n\n" ++ original


fuzzN : (a -> Options -> Outcome) -> List a -> Test
fuzzN fn fuzzTests =
    fuzzTests
        |> List.map fn
        |> Assertions initialOptions
