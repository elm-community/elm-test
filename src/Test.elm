module Test exposing (..)

import Random


type Test
  = Test (Random.Seed -> Outcome)
  | FailWith String Test
  | Runs Int Test
  | DoManyRuns Test
  | Shrink Bool Test
  | Seed Random.Seed Test
  | Batch (List Test)


batch : List Test -> Test
batch =
  Batch


unit : List (() -> Test) -> Test
unit tests =
  List.map (\f -> f ()) tests |> Batch


expectEqual : { expected : a, actually : a } -> Test
expectEqual { expected, actually } =
  Test (\_ -> Out <| expected == actually)


fuzz : Fuzzer a -> List (a -> Test) -> Test
fuzz fuzzer tests =
  List.map (fuzzTest fuzzer) tests |> Batch |> DoManyRuns


fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Test) -> Test
fuzz2 fuzzerA fuzzerB tests =
  List.map (fuzzTest2 fuzzerA fuzzerB) tests |> Batch |> DoManyRuns


fuzzTest : Fuzzer a -> (a -> Test) -> Test
fuzzTest fuzzer runTest =
  -- TODO
  Test (\seed -> Out True)


fuzzTest2 : Fuzzer a -> Fuzzer b -> (a -> b -> Test) -> Test
fuzzTest2 fuzzerA fuzzerB runTest =
  -- TODO
  Test (\seed -> Out True)


type alias PlanItem =
  { seed : Random.Seed
  , failWith : String
  , doShrink : Bool
  , id : String
  , run : Int
  , test : Random.Seed -> Outcome
  }


type alias Plan =
  List PlanItem


type alias Options =
  { seed : Random.Seed
  , failWith : String
  , doShrink : Bool
  , id : String
  , runs : Int
  }


initialOptions : Options
initialOptions =
  Options (Random.initialSeed 42) "Test Failed" True "0" 0


plan : Test -> Plan
plan =
  planHelp initialOptions


planHelp : Options -> Test -> Plan
planHelp opts test =
  case test of
    Test f ->
      List.map
        (\i ->
          PlanItem opts.seed opts.failWith opts.doShrink opts.id i f
        )
        [1..opts.runs]

    FailWith str test ->
      planHelp { opts | failWith = str } test

    Runs i test ->
      planHelp { opts | runs = i } test

    DoManyRuns test ->
      if opts.runs <= 1 then
        planHelp { opts | runs = 100 } test
      else
        planHelp opts test

    Shrink b test ->
      planHelp { opts | doShrink = b } test

    Seed seed test ->
      planHelp { opts | seed = seed } test

    Batch tests ->
      List.indexedMap
        (\i test ->
          planHelp { opts | id = opts.id ++ "." ++ toString i } test
        )
        tests
        |> List.concat


type Outcome
  = Out Bool


type Fuzzer a
  = Fuz
