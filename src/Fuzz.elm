module Fuzz exposing (..)

{-| This is a library of *fuzzers* you can use to supply values to your fuzz
tests. You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a` in two different ways. It
can create them randomly, so that your test's expectations are run against many
values. Fuzzers will often generate edge cases likely to find bugs. If the
fuzzer can make your test fail, it also knows how to "shrink" that failing input
into more minimal examples, some of which might also cause the tests to fail. In
this way, fuzzers can usually find the smallest or simplest input that
reproduces a bug.

## Common Fuzzers
@docs bool, int, string

## Working with Fuzzers
@docs Fuzzer, constant, map, map2, maybe, list

## Tuple Fuzzers
Instead of using a tuple, consider using `fuzzN`.
@docs tuple

## Uncommon Fuzzers
@docs custom, unit

## Not meant to be exported but this is a hack
@docs charGenerator

-}

import Array exposing (Array)
import Char
import Util exposing (..)
import Lazy.List exposing (LazyList)
import Shrink exposing (Shrinker)
import RoseTree exposing (RoseTree(..))
import Random.Pcg as Random exposing (Generator)
import Fuzz.Internal as Internal exposing (Fuzz(..))


{-| The representation of fuzzers is opaque. Conceptually, a `Fuzzer a`
consists of a way to randomly generate values of type `a`, and a way to shrink
those values.
-}
type alias Fuzzer a =
    Internal.Fuzzer a


{-| Build a custom `Fuzzer a` by providing a `Generator a` and a `Shrinker a`.
Generators are defined in [`mgold/elm-random-pcg`](http://package.elm-lang.org/packages/mgold/elm-random-pcg/latest),
which is not core's Random module but has a compatible interface. Shrinkers are
defined in [`elm-community/shrink`](http://package.elm-lang.org/packages/elm-community/shrink/latest/).

Here is an example for a record:

    import Random.Pcg as Random
    import Shrink

    type alias Position =
        { x : Int, y : Int }

    position : Fuzzer Position
    position =
        Fuzz.custom
            (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
            (\{ x, y } -> Shrink.map Position (Shrink.int x) `Shrink.andMap` (Shrink.int y))

Here is an example for a custom union type:

    type Question
        = Name String
        | Age Int

    question =
        let
            generator =
                Random.bool `Random.andThen` (\b ->
                    if b then
                        Random.map Name string.generator
                    else
                        Random.map Age (Random.int 0 120)
                 )

            shrinker question =
                case question of
                    Name n ->
                        Shrink.string n |> Shrink.map Name
                    Age i ->
                        Shrink.int i |> Shrink.map Age
        in
            Fuzz.custom generator shrinker
-}
custom : Generator a -> Shrinker a -> Fuzzer a
custom generator shrinker =
    let
        shrinkTree a =
            Rose a (Lazy.List.map shrinkTree (shrinker a))
    in
        Internal.Fuzzer
            (\noShrink ->
                if noShrink then
                    Gen generator
                else
                    Shrink <| Random.map shrinkTree generator
            )


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    Internal.Fuzzer
        (\noShrink ->
            if noShrink then
                Gen <| Random.constant ()
            else
                Shrink <| Random.constant (RoseTree.singleton ())
        )


{-| A fuzzer for bool values.
-}
bool : Fuzzer Bool
bool =
    Internal.Fuzzer
        (\noShrink ->
            if noShrink then
                Gen <| Random.bool
            else
                Shrink <|
                    Random.map
                        (\b ->
                            if b then
                                RoseTree.Rose True (Lazy.List.singleton (RoseTree.singleton False))
                            else
                                RoseTree.singleton False
                        )
                        Random.bool
        )


{-| A fuzzer for int values.
-}
int : Fuzzer Int
int =
    let
        generator =
            Random.frequency
                [ ( 3, Random.int -50 50 )
                , ( 0.2, Random.constant 0 )
                , ( 1, Random.int 0 (Random.maxInt - Random.minInt) )
                , ( 1, Random.int (Random.minInt - Random.maxInt) 0 )
                ]
    in
        custom generator Shrink.int


charGenerator : Generator Char
charGenerator =
    (Random.map Char.fromCode (Random.int 32 126))


{-| A fuzzer for string values. Generates random printable ascii strings whose
length is between 0 and 10.
-}
string : Fuzzer String
string =
    custom (rangeLengthString 0 10 charGenerator)
        Shrink.string


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe (Internal.Fuzzer baseFuzzer) =
    Internal.Fuzzer <|
        \ft ->
            case baseFuzzer ft of
                Gen gen ->
                    Gen <|
                        Random.map2
                            (\useNothing val ->
                                if useNothing then
                                    Nothing
                                else
                                    Just val
                            )
                            (Random.oneIn 4)
                            gen

                Shrink genTree ->
                    Shrink <|
                        Random.map2
                            (\useNothing tree ->
                                if useNothing then
                                    RoseTree.singleton Nothing
                                else
                                    RoseTree.map Just tree
                            )
                            (Random.oneIn 4)
                            genTree


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Fuzzer a -> Fuzzer (List a)
list (Internal.Fuzzer f) =
    let
        genLength =
            Random.frequency
                [ ( 1, Random.constant 0 )
                , ( 1, Random.constant 1 )
                , ( 3, Random.int 2 10 )
                , ( 2, Random.int 10 100 )
                , ( 0.5, Random.int 100 400 )
                ]
    in
        Internal.Fuzzer
            (\noShrink ->
                case f noShrink of
                    Gen genVal ->
                        Gen <| genLength `Random.andThen` \i -> Random.list i genVal

                    Shrink genTree ->
                        Gen <| genLength `Random.andThen` \i -> Random.list i (Random.map RoseTree.root genTree)
            )


{-| Turn a tuple of fuzzers into a fuzzer of tuples.
-}
tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( Internal.Fuzzer fA, Internal.Fuzzer fB ) =
    Internal.Fuzzer
        (\noShrink ->
            case ( fA noShrink, fB noShrink ) of
                ( Gen genA, Gen genB ) ->
                    Gen <| Random.map2 (,) genA genB

                ( Shrink genRoseA, Shrink genRoseB ) ->
                    Shrink <| Random.map2 (RoseTree.map2 (,)) genRoseA genRoseB

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.tuple" err
        )


{-| Create a fuzzer that only and always returns the value provided, and performs no shrinking. This is hardly random,
and so this function is best used as a helper when creating more complicated fuzzers.
-}
constant : a -> Fuzzer a
constant x =
    Internal.Fuzzer
        (\noShrink ->
            if noShrink then
                Gen (Random.constant x)
            else
                Shrink (Random.constant (RoseTree.singleton x))
        )


{-| Map a function over a fuzzer. This applies to both the generated and the shruken values.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map transform (Internal.Fuzzer f) =
    Internal.Fuzzer
        (\noShrink ->
            case f noShrink of
                Gen genVal ->
                    Gen <| Random.map transform genVal

                Shrink genTree ->
                    Shrink <| Random.map (RoseTree.map transform) genTree
        )


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 transform (Internal.Fuzzer fA) (Internal.Fuzzer fB) =
    Internal.Fuzzer
        (\noShrink ->
            case ( fA noShrink, fB noShrink ) of
                ( Gen genA, Gen genB ) ->
                    Gen <| Random.map2 transform genA genB

                ( Shrink genRoseA, Shrink genRoseB ) ->
                    Shrink <| Random.map2 (RoseTree.map2 transform) genRoseA genRoseB

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.map" err
        )
