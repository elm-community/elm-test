module Fuzz.Internal exposing (Fuzzer(Fuzzer))

import RoseTree exposing (RoseTree)
import Random.Pcg exposing (Generator)


type Fuzzer a
    = Fuzzer (Generator (RoseTree a))
