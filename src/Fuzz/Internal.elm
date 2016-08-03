module Fuzz.Internal exposing (Fuzzer(Fuzzer))

import Shrink exposing (Shrinker)
import Random.Pcg exposing (Generator)


type Fuzzer a
    = Fuzzer
        { generator : Generator a
        , shrinker : Shrinker a
        }
