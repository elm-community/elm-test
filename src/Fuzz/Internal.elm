module Fuzz.Internal exposing (Fuzzer(Fuzzer), Valid, ValidFuzzer, combineValid, invalidReason, map)

import Random.Pcg as Random exposing (Generator)
import RoseTree exposing (RoseTree(Rose))


type Fuzzer a
    = Fuzzer (Valid (ValidFuzzer a))


type alias Valid a =
    Result String a


type alias ValidFuzzer a =
    Generator (RoseTree a)


combineValid : List (Valid a) -> Valid (List a)
combineValid valids =
    case valids of
        [] ->
            Ok []

        (Ok x) :: rest ->
            Result.map ((::) x) (combineValid rest)

        (Err reason) :: _ ->
            Err reason


map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn (Fuzzer fuzzer) =
    (Result.map << Random.map << RoseTree.map) fn fuzzer
        |> Fuzzer


invalidReason : Valid a -> Maybe String
invalidReason valid =
    case valid of
        Ok _ ->
            Nothing

        Err reason ->
            Just reason
