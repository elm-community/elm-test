module Fuzz.Internal exposing (Fuzzer(Fuzzer), Fuzz(..), unpackGenVal, unpackGenTree)

import RoseTree exposing (RoseTree)
import Random.Pcg exposing (Generator)


type Fuzzer a
    = Fuzzer (Bool -> Fuzz a)


type Fuzz a
    = Gen (Generator a)
    | Shrink (Generator (RoseTree a))


unpackGenVal : Fuzzer a -> Generator a
unpackGenVal (Fuzzer g) =
    case g True of
        Gen genVal ->
            genVal

        err ->
            Debug.crash "This shouldn't happen: Fuzz.Internal.unpackGenVal" err


unpackGenTree : Fuzzer a -> Generator (RoseTree a)
unpackGenTree (Fuzzer g) =
    case g False of
        Shrink genTree ->
            genTree

        err ->
            Debug.crash "This shouldn't happen: Fuzz.Internal.unpackGenTree" err
