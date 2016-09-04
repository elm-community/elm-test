module Fuzz.Internal exposing (Fuzzer(Fuzzer, InvalidFuzzer), Fuzz(Gen, Shrink), unpackGenVal, unpackGenTree)

import RoseTree exposing (RoseTree)
import Random.Pcg exposing (Generator)


type Fuzzer a
    = Fuzzer (Bool -> Fuzz a)
    | InvalidFuzzer String


type Fuzz a
    = Gen (Generator a)
    | Shrink (Generator (RoseTree a))


unpackGenVal : (Bool -> Fuzz a) -> Generator a
unpackGenVal g =
    case g True of
        Gen genVal ->
            genVal

        err ->
            Debug.crash "This shouldn't happen: Fuzz.Internal.unpackGenVal" err


unpackGenTree : (Bool -> Fuzz a) -> Generator (RoseTree a)
unpackGenTree g =
    case g False of
        Shrink genTree ->
            genTree

        err ->
            Debug.crash "This shouldn't happen: Fuzz.Internal.unpackGenTree" err
