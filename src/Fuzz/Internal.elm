module Fuzz.Internal exposing (Fuzzer(Fuzzer), Fuzz(..))

import RoseTree exposing (RoseTree)
import Random.Pcg exposing (Generator)


type Fuzzer a
    = Fuzzer (Bool -> Fuzz a)


type Fuzz a
    = Gen (Generator a)
    | Shrink (Generator (RoseTree a))
