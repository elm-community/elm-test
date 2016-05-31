module Test exposing (..)

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


type Test
    = Test Options TestTree


type Fuzzer a
    = -- TODO: shrinking
      Fuzzer (Random.Generator a)


batch : List Test -> Test
batch tests =
    Test { onFail = "Batch failed:", runs = 1, doShrink = False } (Batch tests)


onFail : String -> Test -> Test
onFail str (Test opts tree) =
    Test { opts | onFail = str } tree


runs : Int -> Test -> Test
runs int (Test opts tree) =
    Test { opts | runs = int } tree


unit : List (() -> Test) -> Test
unit tests =
    Test { onFail = "Unit test suite failed:", runs = 1, doShrink = False }
        <| Group
        <| List.map (\t _ -> t ()) tests


fuzz : Fuzzer a -> List (a -> Test) -> Test
fuzz (Fuzzer gen) fuzzTests =
    Test { onFail = "Fuzz test suite failed:", runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTests =
                        Random.list runs gen |> Random.map (List.map (\arg _ -> fuzzTest arg))

                    opts =
                        { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Test) -> Test
fuzz2 (Fuzzer genA) (Fuzzer genB) fuzzTests =
    Test { onFail = "Fuzz test suite failed:", runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTuple =
                        Random.map2 (,) genA genB

                    genTests =
                        Random.list runs genTuple |> Random.map (List.map (\( a, b ) _ -> fuzzTest a b))

                    opts =
                        { onFail = "Fuzz test failed:", runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


assertEqual : { expected : a, actually : a } -> Test
assertEqual { expected, actually } =
    Test { onFail = "Test failed:", runs = 1, doShrink = False }
        <| Thunk
            (\_ ->
                if expected == actually then
                    []
                else
                    [ ( "Expected", toString expected ), ( "Actually", toString actually ) ]
            )


type ResultTree
    = Leaf String Outcome
    | Branch String (List ResultTree)


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
