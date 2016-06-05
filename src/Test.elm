module Test exposing (Test, ResultTree, Outcome, toRunners, unit, fuzz, fuzz2, assertEqual, onFail, runs)

{-|

@docs Test, ResultTree, Outcome, unit, fuzz, fuzz2, toRunners, assertEqual, onFail, runs
-}

import Fuzzer exposing (Fuzzer)
import Random.Pcg as Random


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


{-| TODO: docs
-}
fuzz : Fuzzer a -> List (a -> Assertion) -> Test
fuzz { generator } fuzzTests =
    Debug.crash "TODO"


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



--Test { onFail = "Fuzz test suite", runs = 100, doShrink = True }
--    <| Fuzz
--    <| (flip List.map) fuzzTests
--        (\fuzzTest ( seed, runs, doShrink ) ->
--            let
--                genTests =
--                    Random.list runs generator |> Random.map (List.map (\arg _ -> fuzzTest arg))
--                opts =
--                    { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
--            in
--                Random.step genTests seed |> fst |> Fuzz |> Test opts
--        )


{-| TODO: docs
-}
fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Assertion) -> Test
fuzz2 fuzzA fuzzB fuzzTests =
    Debug.crash "TODO"



--Test
--{ onFail = "Fuzz test suite failed:", runs = 100, doShrink = True }
--<| Fuzz
--<| (flip List.map) fuzzTests
--    (\fuzzTest ( seed, runs, doShrink ) ->
--        let
--            genTuple =
--                Random.map2 (,) fuzzA.generator fuzzB.generator
--            genTests =
--                Random.list runs genTuple |> Random.map (List.map (\( a, b ) _ -> fuzzTest a b))
--            opts =
--                { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
--        in
--            Random.step genTests seed |> fst |> Fuzz |> Test opts
--    )


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



--runWithSeed seed (Test opts tree) =
--    let
--        unfiltered =
--            Debug.crash "TODO"
--case tree of
--    Thunk thunk ->
--        Leaf opts.onFail (thunk ())
--    Group thunks ->
--        Branch opts.onFail <| List.map (\thunk -> runWithSeed seed (thunk ())) thunks
--    FuzzGroup randThunks ->
--        Random.list (List.length randThunks) Random.independentSeed
--            |> Random.map (List.map2 (\randThunk seed -> randThunk ( seed, opts.runs, opts.doShrink ) |> runWithSeed seed) randThunks)
--            |> (flip Random.step) seed
--            |> fst
--            |> Branch opts.onFail
--    Batch tests ->
--        Random.list (List.length tests) Random.independentSeed
--            |> (flip Random.step) seed
--            |> fst
--            |> List.map2 (\test seed -> runWithSeed seed test) tests
--            |> Branch opts.onFail
--in
--    filterSuccesses unfiltered |> Maybe.withDefault (Branch "No failures" [])


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
