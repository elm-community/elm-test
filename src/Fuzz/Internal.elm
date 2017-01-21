module Fuzz.Internal exposing (Fuzzer(Fuzzer), Fuzz(..), unpackGenVal, unpackGenTree, promote)

import RoseTree exposing (RoseTree)
import Random.Pcg as Random exposing (Generator)


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


{-| Used by Fuzz.function - potentially informative links:

* Issue - https://github.com/elm-community/elm-test/issues/109
* Paper - http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
* QuickCheck implementation - https://github.com/nick8325/quickcheck/blob/master/Test/QuickCheck/Gen/Unsafe.hs
-}
promote : (a -> Fuzzer b) -> Fuzzer (a -> b)
promote toFuzzer =
    let
        fromBool : Bool -> Fuzz (a -> b)
        fromBool single =
            if single then
                Gen (toGenerator toFuzzer)
            else
                Debug.crash "TODO handle shrinking"
    in
        Fuzzer fromBool


{-| CAUTION: uses unpackGenVal!
-}
toGenerator : (a -> Fuzzer b) -> Generator (a -> b)
toGenerator toFuzzer =
    Random.independentSeed
        |> Random.andThen
            (\seed ->
                Random.constant
                    (\a ->
                        seed
                            |> Random.step (unpackGenVal (toFuzzer a))
                            |> Tuple.first
                    )
            )


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
