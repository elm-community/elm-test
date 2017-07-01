module Fuzz.Internal exposing (Fuzzer(Fuzzer), Valid, ValidFuzzer)

import Random.Pcg as Random exposing (Generator)
import RoseTree exposing (RoseTree(Rose))


type Fuzzer a
    = Fuzzer (Valid (ValidFuzzer a))


type alias Valid a =
    Result String a


type alias ValidFuzzer a =
    Generator (RoseTree a)
