module Test exposing (..)

import Random


type Test
  = Test (() -> List ( String, String ))
  | Group (List (Random.Seed -> Test))
  | Batch (List Test)
  | FailWith String Test
  | Runs Int Test
  | DoManyRuns Test
  | Shrink Bool Test


batch : List Test -> Test
batch =
  Batch


onFail : String -> Test -> Test
onFail =
  FailWith


unit : List (() -> Test) -> Test
unit tests =
  List.map (\f seed -> f ()) tests |> Group


fuzz : Fuzzer a -> List (a -> Test) -> Test
fuzz fuzzer tests =
  List.map (fuzzTest fuzzer) tests |> Group |> DoManyRuns


fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Test) -> Test
fuzz2 fuzzerA fuzzerB tests =
  List.map (fuzzTest2 fuzzerA fuzzerB) tests |> Batch |> DoManyRuns


assertEqual : { expected : a, actually : a } -> Test
assertEqual { expected, actually } =
  Test
    (\_ ->
      if expected == actually then
        []
      else
        [ ( "Expected", toString expected ), ( "Actually", toString actually ) ]
    )


fuzzTest : Fuzzer a -> (a -> Test) -> Random.Seed -> Test
fuzzTest (Fuzzer gen) f seed =
  -- TODO
  Test (\() -> [])


fuzzTest2 : Fuzzer a -> Fuzzer b -> (a -> b -> Test) -> Test
fuzzTest2 fuzzerA fuzzerB runTest =
  -- TODO
  Test (\() -> [])



{-

   type alias PlanItem =
     { seed : Random.Seed
     , failWith : String
     , doShrink : Bool
     , id : String
     , run : Int
     , test : Random.Seed -> Test
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

       Batch tests ->
         List.indexedMap
           (\i test ->
             planHelp { opts | id = opts.id ++ "." ++ toString i } test
           )
           tests
           |> List.concat

-}


type Fuzzer a
  = Fuzzer (Random.Generator a)


{-| Stubbed fuzzer
-}
string : Fuzzer String
string =
  Fuzzer
    <| Random.map
        (\b ->
          if b then
            "foo"
          else
            "bar"
        )
        Random.bool
