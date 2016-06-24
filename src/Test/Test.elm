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
            if runs < 1 then
                [ Fail ("Fuzz test run count must be at least 1, not " ++ toString runs) ]
            else
                let
                    runWithInput val =
                        ( val, getOutcome val )

                    generators =
                        fuzzer.generator
                            |> Random.list runs
                            |> Random.map (List.map runWithInput)

                    ( pairs, _ ) =
                        Random.step generators seed
                in
                    -- Make sure if we passed, we don't do any more work.
                    if List.all (\( _, outcome ) -> outcome == Pass) pairs then
                        [ Pass ]
                    else
                        let
                            shrink ( val, outcome ) =
                                if isFail outcome then
                                    let
                                        shrunkenVal =
                                            Shrink.shrink (getOutcome >> isFail) fuzzer.shrinker val
                                    in
                                        ( Just (toString shrunkenVal), getOutcome shrunkenVal )
                                else
                                    ( Just (toString val), outcome )
                        in
                            pairs
                                |> List.map shrink
                                |> dedupe
                                |> List.map formatExpectation
    in
        Labeled desc (Test run)


formatExpectation : ( Maybe String, Expectation ) -> Expectation
formatExpectation ( input, outcome ) =
    Test.Expectation.formatFailure (prependInput input) outcome


dedupe : List ( Maybe String, a ) -> List ( Maybe String, a )
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
