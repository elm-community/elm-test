module Test exposing (Test, ResultTree, unit, fuzz, fuzz2, assertEqual, batch, onFail, runs, runWithSeed)

{-|

@docs Test, ResultTree, unit, fuzz, fuzz2, assertEqual, batch, onFail, runs, runWithSeed
-}

import Fuzzer exposing (Fuzzer)
import Random.Pcg as Random


-- none of the types below will be exported, except Test which will be opaque


type alias Outcome =
    List ( String, String )


{-| A TestTree is either
   * A leaf thunk that yields an outcome
   * A node whose children are lazy thunks
   * A node whose children are lazy and randomized thunks
   * A node whose children are already evaluated
-}
type TestTree
    = Thunk (() -> Outcome)
    | Group (List (() -> Test))
    | FuzzGroup (List (( Random.Seed, Int, Bool ) -> Test))
      -- seed, runs, doShrink; make record?
    | Batch (List Test)


type alias Options =
    { onFail : String
    , runs : Int
    , doShrink : Bool
    }


{-| TODO: docs
-}
type Test
    = Test Options TestTree


{-| TODO: docs
-}
batch : List Test -> Test
batch tests =
    Test { onFail = "Batch failed:", runs = 1, doShrink = False } (Batch tests)


{-| TODO: docs
-}
onFail : String -> Test -> Test
onFail str (Test opts tree) =
    Test { opts | onFail = str } tree


{-| TODO: docs
-}
runs : Int -> Test -> Test
runs int (Test opts tree) =
    Test { opts | runs = int } tree


{-| TODO: docs
-}
unit : List (() -> Test) -> Test
unit tests =
    Test { onFail = "Unit test suite failed:", runs = 1, doShrink = False }
        <| Group
        <| List.map (\t _ -> t ()) tests


{-| TODO: docs
-}
fuzz : Fuzzer a -> List (a -> Test) -> Test
fuzz { generator } fuzzTests =
    Test { onFail = "Fuzz test suite failed:", runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTests =
                        Random.list runs generator |> Random.map (List.map (\arg _ -> fuzzTest arg))

                    opts =
                        { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


{-| TODO: docs
-}
fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Test) -> Test
fuzz2 fuzzA fuzzB fuzzTests =
    Test { onFail = "Fuzz test suite failed:", runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTuple =
                        Random.map2 (,) fuzzA.generator fuzzB.generator

                    genTests =
                        Random.list runs genTuple |> Random.map (List.map (\( a, b ) _ -> fuzzTest a b))

                    opts =
                        { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


{-| TODO: docs
-}
assertEqual : { expected : a, actually : a } -> Test
assertEqual { expected, actually } =
    Test { onFail = "Test failed:", runs = 1, doShrink = False }
        <| Thunk
            (\_ ->
                if expected == actually then
                    []
                else
                    [ ( "Expected: ", toString expected ), ( "Actual:   ", toString actually ) ]
            )


{-| TODO: docs
-}
type ResultTree
    = Leaf String Outcome
    | Branch String (List ResultTree)


{-| TODO: docs
-}
runWithSeed : Random.Seed -> Test -> ResultTree
runWithSeed seed (Test opts tree) =
    let
        unfiltered =
            case tree of
                Thunk thunk ->
                    Leaf opts.onFail (thunk ())

                Group thunks ->
                    Branch opts.onFail <| List.map (\thunk -> runWithSeed seed (thunk ())) thunks

                FuzzGroup randThunks ->
                    Random.list (List.length randThunks) Random.independentSeed
                        |> Random.map (List.map2 (\randThunk seed -> randThunk ( seed, opts.runs, opts.doShrink ) |> runWithSeed seed) randThunks)
                        |> (flip Random.step) seed
                        |> fst
                        |> Branch opts.onFail

                Batch tests ->
                    Random.list (List.length tests) Random.independentSeed
                        |> (flip Random.step) seed
                        |> fst
                        |> List.map2 (\test seed -> runWithSeed seed test) tests
                        |> Branch opts.onFail
    in
        filterSuccesses unfiltered |> Maybe.withDefault (Branch "No failures" [])


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
