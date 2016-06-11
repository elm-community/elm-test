module Test exposing (Test, Outcome, RunnerConfig, isPass, unit, failWith, toFailures, it, formatFailure, pass, fail, isFail, toRunners, describe, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| Testing

@docs Test, Outcome, RunnerConfig, pass, fail, it, unit, failWith, toFailures, isPass, isFail, formatFailure, toRunners, describe, fuzz, fuzz2, fuzz3, fuzz4, fuzz5
-}

import Random.Pcg as Random
import Dict
import Shrink
import Random.Pcg as Random exposing (Generator)
import Fuzzer exposing (Fuzzer)


{-| A batch of Tests which have yet to be evaluated. Execution order is not
guaranteed.

Use [`toRunners`](#toRunners) to convert a `Suite` into a list of
`() -> Test` functions, which can then be evaluated.
-}
type alias Test =
    RunnerConfig -> Outcome


type alias Suite =
    List Test


{-| TODO document
-}
type RunnerConfig
    = RunnerConfig Random.Seed Int


{-| Apply a description to a `Test`.

-- TODO give a code example.
-}
it : String -> Test -> List Test
it str getOutcome =
    let
        run : RunnerConfig -> Outcome
        run context =
            case getOutcome context of
                Pass records ->
                    records
                        |> List.map (\{ context } -> { context = str :: context })
                        |> Pass

                Fail records ->
                    records
                        |> List.map (\record -> { record | context = str :: record.context })
                        |> Fail
    in
        [ run ]


{-| Turn a `Suite` into a list of thunks that can be run to produce Tests.

-- TODO code example
-}
toRunners : Random.Seed -> Int -> List Test -> List (() -> Outcome)
toRunners seed runs =
    let
        config =
            RunnerConfig seed runs
    in
        List.map (\run _ -> run config)


{-| The result of a single test run. This can either be a [`pass`](#pass) or
[`fail`](#fail).

Use [`toFailures`](#toFailures) to convert an `Outcome` into appropriately
contextualized failure messages.
-}
type Outcome
    = Pass (List { context : List String })
    | Fail (List { context : List String, message : String })


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (failWith)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> failWith "thought they'd be the same"
        |> Test.toFailures
        -- Just { messages = [ "thought they'd be the same" ], context = [] }
-}
failWith : String -> Outcome -> Outcome
failWith str =
    formatFailure (\_ -> str)


{-| A [`Test`](#Test) which failed with the given message.

-- TODO code sample
-}
fail : String -> Outcome
fail str =
    Fail [ { message = str, context = [] } ]


{-| A [`Test`](#Test) which passed.

-- TODO code sample
-}
pass : Outcome
pass =
    Pass [ { context = [] } ]


{-| Return contextualized failure messages from the given [`Test`](#Test).

Note that fuzz tests may return multiple failure messages from a single `Test`!

-- TODO code sample
-}
toFailures : Outcome -> List { context : List String, failure : Maybe String }
toFailures test =
    case test of
        Pass records ->
            records
                |> List.map (\{ context } -> { context = context, failure = Nothing })

        Fail records ->
            records
                |> List.map (\{ context, message } -> { context = context, failure = Just message })


{-| Format all the failure messages in a given `Test`.

-- TODO code sample
-}
formatFailure : (String -> String) -> Outcome -> Outcome
formatFailure format outcome =
    case outcome of
        Fail records ->
            records
                |> List.map (\record -> { record | message = format record.message })
                |> Fail

        Pass _ ->
            outcome


{-| Apply a description to a list of tests.

-- TODO give a code example.
-}
describe : String -> List (List Test) -> List Test
describe desc =
    List.concatMap (List.concatMap (it desc))


{-| TODO docs
-}
unit : Test -> List Test
unit test =
    [ test ]


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
    -> (a -> Outcome)
    -> Test
fuzz fuzzer =
    fuzzTest fuzzer


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple`.


-- TODO code sample
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> (a -> b -> Outcome)
    -> Test
fuzz2 fuzzA fuzzB =
    let
        fuzzer =
            Fuzzer.tuple ( fuzzA, fuzzB )
    in
        uncurry >> fuzzTest fuzzer


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple3`.

-- TODO code sample
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> (a -> b -> c -> Outcome)
    -> Test
fuzz3 fuzzA fuzzB fuzzC =
    let
        fuzzer =
            Fuzzer.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        uncurry3 >> fuzzTest fuzzer


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple4`.

-- TODO code sample
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> (a -> b -> c -> d -> Outcome)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD =
    let
        fuzzer =
            Fuzzer.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        uncurry4 >> fuzzTest fuzzer


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
    -> (a -> b -> c -> d -> e -> Outcome)
    -> Test
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        fuzzer =
            Fuzzer.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        uncurry5 >> fuzzTest fuzzer



-- INTERNAL HELPERS --


defaults : { runs : Int, doShrink : Bool, seed : Random.Seed }
defaults =
    { runs = 100
    , doShrink = True
    , seed = Random.initialSeed 42
    }


fuzzTest : Fuzzer a -> (a -> Outcome) -> Test
fuzzTest fuzzer getOutcome =
    let
        run (RunnerConfig seed runs) =
            let
                runWithInput val =
                    let
                        outcome =
                            getOutcome val

                        shrunkenVal =
                            if isFail outcome then
                                Shrink.shrink (getOutcome >> isFail) fuzzer.shrinker val
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
                    |> List.map formatOutcome
                    |> concatOutcomes
    in
        run


{-| TODO document
-}
isPass : Outcome -> Bool
isPass outcome =
    case outcome of
        Pass _ ->
            True

        Fail _ ->
            False


{-| TODO document
-}
isFail : Outcome -> Bool
isFail =
    not << isPass


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e


concatOutcomes : List Outcome -> Outcome
concatOutcomes =
    concatOutcomesHelp pass


concatOutcomesHelp : Outcome -> List Outcome -> Outcome
concatOutcomesHelp result outcomes =
    case outcomes of
        [] ->
            result

        ((Pass records) as first) :: rest ->
            case result of
                Pass newRecords ->
                    concatOutcomesHelp (Pass (records ++ newRecords)) rest

                Fail newRecords ->
                    concatOutcomesHelp (Fail newRecords) rest

        ((Fail records) as first) :: rest ->
            case result of
                Pass _ ->
                    concatOutcomesHelp first rest

                Fail newRecords ->
                    concatOutcomesHelp (Fail (records ++ newRecords)) rest


formatOutcome : ( Maybe String, Outcome ) -> Outcome
formatOutcome ( input, outcome ) =
    formatFailure (prependInput input) outcome


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "Input: " ++ str ++ "\n\n" ++ original
