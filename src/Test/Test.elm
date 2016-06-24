module Test.Test exposing (Test(..), fuzzTest)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Dict
import Shrink
import Fuzz exposing (Fuzzer)


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


fuzzTest : String -> Fuzzer a -> (a -> Expectation) -> Test
fuzzTest desc fuzzer getOutcome =
    let
        run seed runs =
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
                    |> List.map formatExpectation
    in
        Labeled desc (Test run)


formatExpectation : ( Maybe String, Expectation ) -> Expectation
formatExpectation ( input, outcome ) =
    Test.Expectation.formatFailure (prependInput input) outcome


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "► Given " ++ str ++ "\n\n" ++ original


isFail : Expectation -> Bool
isFail =
    (/=) Test.Expectation.Pass
