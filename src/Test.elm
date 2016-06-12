module Test exposing (Test, Outcome, Suite(..), unit, failWith, toFailures, it, formatFailure, pass, fail, describe, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| Testing

@docs Test, Outcome, Suite, pass, fail, it, unit, failWith, toFailures, formatFailure, describe, fuzz, fuzz2, fuzz3, fuzz4, fuzz5
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
    { runs : Int, seed : Random.Seed } -> Outcome


{-| TODO document
-}
type Suite
    = Suite (List Test)
    | Labeled String Suite
    | Batch (List Suite)


{-| Apply a description to a `Test`.

-- TODO give a code example.
-}
it : String -> Test -> Suite
it str test =
    Labeled str (Suite [ test ])


{-| The result of a single test run. This can either be a [`pass`](#pass) or
[`fail`](#fail).

Use [`toFailures`](#toFailures) to convert an `Outcome` into appropriately
contextualized failure messages.
-}
type Outcome
    = Pass
    | Fail (List String)


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
    Fail [ str ]


{-| A [`Test`](#Test) which passed.

-- TODO code sample
-}
pass : Outcome
pass =
    Pass


{-| Return contextualized failure messages from the given [`Test`](#Test).

Note that fuzz tests may return multiple failure messages from a single `Test`!

-- TODO code sample
-}
toFailures : Outcome -> Maybe (List String)
toFailures outcome =
    case outcome of
        Pass ->
            Nothing

        Fail failures ->
            Just failures


{-| Format all the failure messages in a given `Test`.

-- TODO code sample
-}
formatFailure : (String -> String) -> Outcome -> Outcome
formatFailure format outcome =
    case outcome of
        Fail messages ->
            messages
                |> List.map format
                |> Fail

        Pass ->
            outcome


{-| Apply a description to a list of tests.

-- TODO give a code example.
-}
describe : String -> List Suite -> Suite
describe desc =
    Batch >> Labeled desc


{-| TODO docs
-}
unit : Test -> Suite
unit test =
    Suite [ test ]


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
        run { seed, runs } =
            let
                runWithInput val =
                    let
                        outcome =
                            getOutcome val

                        shrunkenVal =
                            if outcome /= pass then
                                Shrink.shrink (getOutcome >> (/=) pass) fuzzer.shrinker val
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

        Pass :: rest ->
            case result of
                Pass ->
                    concatOutcomesHelp result rest

                (Fail _) as failure ->
                    concatOutcomesHelp failure rest

        ((Fail newFailures) as first) :: rest ->
            case result of
                Pass ->
                    concatOutcomesHelp first rest

                Fail oldFailures ->
                    concatOutcomesHelp (Fail (oldFailures ++ newFailures)) rest


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
