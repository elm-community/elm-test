module Test exposing (Test, ResultTree, Outcome, toRunners, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, assertEqual, onFail, runs)

{-|

@docs Test, ResultTree, Outcome, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, toRunners, assertEqual, onFail, runs
-}

import Fuzzer exposing (Fuzzer)
import Random.Pcg as Random exposing (Generator)


-- none of the types below will be exported, except Test which will be opaque


{-| The outcome from running a single test.
-}
type alias Outcome =
    List String


type alias Options =
    { onFail : List String
    , runs : Maybe Int
    , doShrink : Maybe Bool
    , seed : Maybe Random.Seed
    }


defaultOptions : Options
defaultOptions =
    { onFail = [], runs = Nothing, doShrink = Nothing, seed = Nothing }


{-| A Test is either
   * A list of thunks, each of which returns an assertion
   * A list of child tests
-}
type Test
    = Assertions Options (List (() -> Assertion))
    | Batch Options (List Test)


{-| TODO: docs
-}
type Assertion
    = Assertion (Random.Seed -> Int -> Bool -> Outcome)


mergeOptions : Options -> Options -> Options
mergeOptions child parent =
    { onFail = parent.onFail ++ child.onFail
    , runs = Maybe.oneOf [ child.runs, parent.runs ]
    , doShrink = Maybe.oneOf [ child.doShrink, parent.doShrink ]
    , seed = Maybe.oneOf [ child.seed, parent.seed ]
    }


{-| Turn a `Test` into a list of thunks that can be run to produce outcomes.
-}
toRunners : Random.Seed -> Test -> List (() -> Outcome)
toRunners seed =
    toRunnersHelp { defaultOptions | seed = Just seed }


toRunnersHelp : Options -> Test -> List (() -> Outcome)
toRunnersHelp baseOpts test =
    case test of
        Assertions opts thunks ->
            List.map (thunkToOutcome (mergeOptions opts baseOpts)) thunks

        Batch opts suites ->
            List.concatMap (toRunnersHelp (mergeOptions opts baseOpts)) suites


thunkToOutcome : Options -> (() -> Assertion) -> () -> Outcome
thunkToOutcome opts thunk _ =
    case thunk () of
        Assertion run ->
            run (Maybe.withDefault defaultSeed opts.seed)
                (Maybe.withDefault 1 opts.runs)
                (Maybe.withDefault True opts.doShrink)


defaultSeed : Random.Seed
defaultSeed =
    Random.initialSeed 42


{-| TODO: docs
-}
onFail : String -> Assertion -> Assertion
onFail str (Assertion oldRun) =
    let
        -- Run the original assertion, then replace any failure output with str.
        run seed runs doShrink =
            if List.isEmpty (oldRun seed runs doShrink) then
                []
            else
                [ str ]
    in
        Assertion run


{-| TODO: docs
-}
runs : Int -> Test -> Test
runs count test =
    case test of
        Assertions opts thunks ->
            Assertions { opts | runs = Just count } thunks

        Batch opts tests ->
            Batch { opts | runs = Just count } tests


{-| TODO: docs
-}
unit : List (() -> Assertion) -> Test
unit =
    Assertions defaultOptions


{-| TODO docs
-}
fuzz :
    Fuzzer a
    -> List (a -> Assertion)
    -> Test
fuzz { generator } fuzzTests =
    Assertions defaultOptions (List.map (fuzzToThunk generator) fuzzTests)


fuzzN : (a -> () -> Assertion) -> List a -> Test
fuzzN fn fuzzTests =
    fuzzTests
        |> List.map fn
        |> Assertions defaultOptions


{-| TODO docs
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> List (a -> b -> Assertion)
    -> Test
fuzz2 fuzzA fuzzB =
    let
        gen =
            Random.map2 (,)
                fuzzA.generator
                fuzzB.generator
    in
        fuzzN (uncurry >> fuzzToThunk gen)


{-| TODO docs
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> List (a -> b -> c -> Assertion)
    -> Test
fuzz3 fuzzA fuzzB fuzzC =
    let
        gen =
            Random.map3 (,,)
                fuzzA.generator
                fuzzB.generator
                fuzzC.generator
    in
        fuzzN (uncurry3 >> fuzzToThunk gen)


{-| TODO docs
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> List (a -> b -> c -> d -> Assertion)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD =
    let
        gen =
            Random.map4 (,,,)
                fuzzA.generator
                fuzzB.generator
                fuzzC.generator
                fuzzD.generator
    in
        fuzzN (uncurry4 >> fuzzToThunk gen)


{-| TODO docs
-}
fuzz5 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> List (a -> b -> c -> d -> e -> Assertion)
    -> Test
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        gen =
            Random.map5 (,,,,)
                fuzzA.generator
                fuzzB.generator
                fuzzC.generator
                fuzzD.generator
                fuzzE.generator
    in
        fuzzN (uncurry5 >> fuzzToThunk gen)


fuzzToThunk : Generator a -> (a -> Assertion) -> () -> Assertion
fuzzToThunk generator runAssert _ =
    let
        run seed runs doShrink =
            let
                -- testRuns : Generator (List a)
                testRuns =
                    generator
                        |> Random.list runs
                        |> Random.map (List.map runAssert)
            in
                Random.step testRuns seed
                    |> resolveAssertions
    in
        Assertion run


resolveAssertions : ( List Assertion, Random.Seed ) -> Outcome
resolveAssertions ( assertions, seed ) =
    List.concatMap (resolveAssertion seed) assertions


resolveAssertion : Random.Seed -> Assertion -> Outcome
resolveAssertion seed (Assertion run) =
    run seed 1 False


{-| Run a list of tests.

See [`describe`](#describe) for running tests with a descriptive string.
-}
batch : List Test -> Test
batch =
    Batch defaultOptions


{-| Run a list of tests, associated with the given description.
-}
describe : String -> List Test -> Test
describe desc =
    Batch { defaultOptions | onFail = [ desc ] }


{-| TODO: docs
-}
assertEqual : { expected : a, actual : a } -> Assertion
assertEqual { expected, actual } =
    let
        run _ _ _ =
            if expected == actual then
                []
            else
                [ "Expected: " ++ toString expected, "Actual:   " ++ toString actual ]
    in
        Assertion run


{-| TODO: docs
-}
type ResultTree
    = Leaf String Outcome
    | Branch String (List ResultTree)


filterSuccesses : ResultTree -> Maybe ResultTree
filterSuccesses rt =
    case rt of
        Leaf _ xs ->
            if List.isEmpty xs then
                Nothing
            else
                Just rt

        Branch onFail results ->
            let
                filtered =
                    List.filterMap filterSuccesses results
            in
                if List.isEmpty filtered then
                    Nothing
                else
                    Just (Branch onFail filtered)


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e
