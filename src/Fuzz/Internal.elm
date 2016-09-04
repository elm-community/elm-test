module Fuzz.Internal exposing (Fuzzer(Fuzzer), Fuzz(..), unpackGenVal, unpackGenTree)

import RoseTree exposing (RoseTree)
import Random.Pcg exposing (Generator)


{- Fuzzers as opt-in RoseTrees

   In the beginning, a Fuzzer was a record of a random generator and a shrinker.
   And it was bad, because that makes it impossible to shrink any value created by
   mapping over other values. But at least it was fast, and shrinking worked well.

   On the second branch, we created RoseTrees, where every randomly-generated value
   also kept a lazy list of shrunken values, which also keep shrunken forms of
   themselves. This allows for advanced maps to be implemented, but it was slow.

   On the third branch, we realized that we shouldn't have to pay for shrinking in
   the common case of a passing test. So Fuzzers became a function from a boolean
   to either another union type. If the function is passed True, it returns a
   Generator of a single value; if False, a Generator of a RoseTree of values.
   (This is almost certainly dependent types leaning on Debug.crash.) The root of
   the RoseTree must equal the single value. Thus the testing harness "opts-in" to
   producing a rosetree, doing so only after the single-value generator has caused
   a test to fail.

   These two optimizations make the Fuzzer code rather hard to understand, but
   allow it to offer a full mapping API, be fast for passing tests, and provide
   shrunken values for failing tests.
-}


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
