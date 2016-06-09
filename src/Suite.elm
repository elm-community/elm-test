module Suite exposing (Suite, toRunners, batch, describe, withRuns, withSeed, withShrink, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| A collection of Tests.

@docs Suite, toRunners, batch, describe, withRuns, withSeed, withShrink, unit, fuzz, fuzz2, fuzz3, fuzz4, fuzz5
-}

import Dict
import Shrink
import Random.Pcg as Random exposing (Generator)
import Test exposing (Test)
import Fuzzer exposing (Fuzzer)


{-| A Suite is either
   * A list of thunks, each of which returns an Test
   * A batch of these Suites
-}
type Suite
    = Assertions Options (List (Options -> Test))
    | Batch (List Suite)


{-| Turn a `Suite` into a list of thunks that can be run to produce Tests.
-}
toRunners : Random.Seed -> Suite -> List (() -> Test)
toRunners seed suite =
    case suite of
        Assertions opts thunks ->
            List.map (toTest seed opts) thunks

        Batch suites ->
            List.concatMap (toRunners seed) suites


{-| TODO: docs
-}
withRuns : Int -> Suite -> Suite
withRuns runs =
    mapOptions (\opts -> { opts | runs = Maybe.oneOf [ opts.runs, Just runs ] })


{-| TODO: docs
-}
withSeed : Random.Seed -> Suite -> Suite
withSeed seed =
    mapOptions (\opts -> { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] })


{-| TODO: docs
-}
withShrink : Bool -> Suite -> Suite
withShrink doShrink =
    mapOptions (\opts -> { opts | doShrink = Maybe.oneOf [ opts.doShrink, Just doShrink ] })


{-| Run a list of Suites.

See [`describe`](#describe) for running Suites with a descriptive string.
-}
batch : List Suite -> Suite
batch =
    Batch


{-| Apply a description string to a Suite.
-}
describe : String -> (a -> Suite) -> a -> Suite
describe desc getSuite arg =
    mapOptions (prependFail desc) (getSuite arg)


{-| TODO: docs
-}
unit : List (() -> Test) -> Suite
unit fns =
    fns
        -- TODO refactor this
        |>
            List.map (\fn -> (\_ -> fn ()))
        |>
            Assertions initialUnitOptions


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
    -> List (a -> Test)
    -> Suite
fuzz fuzzer fuzzSuites =
    Assertions initialOptions (List.map (fuzzToThunk fuzzer) fuzzSuites)


{-| TODO docs
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
        fuzzN (uncurry >> fuzzToThunk fuzzer)


{-| TODO docs
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
        fuzzN (uncurry3 >> fuzzToThunk fuzzer)


{-| TODO docs
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
        fuzzN (uncurry4 >> fuzzToThunk fuzzer)


{-| TODO docs
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
        fuzzN (uncurry5 >> fuzzToThunk fuzzer)



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


toTest : Random.Seed -> Options -> (Options -> Test) -> () -> Test
toTest seed opts getAssertion _ =
    let
        test =
            getAssertion { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] }
    in
        List.foldr Test.it test opts.onFail


prependFail : String -> Options -> Options
prependFail str opts =
    { opts | onFail = str :: opts.onFail }


mapOptions : (Options -> Options) -> Suite -> Suite
mapOptions translate suite =
    case suite of
        Assertions opts thunks ->
            Assertions (translate opts) thunks

        Batch suites ->
            suites
                |> List.map (mapOptions translate)
                |> Batch


fuzzToThunk : Fuzzer a -> (a -> Test) -> Options -> Test
fuzzToThunk fuzzer runAssert opts =
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
                    if doShrink && not (isSuccess test) then
                        Shrink.shrink (runAssert >> isSuccess >> not) fuzzer.shrinker val
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
            |> Test.concat


fuzzN : (a -> Options -> Test) -> List a -> Suite
fuzzN fn fuzzSuites =
    fuzzSuites
        |> List.map fn
        |> Assertions initialOptions


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


isSuccess : Test -> Bool
isSuccess =
    (==) Test.succeed
