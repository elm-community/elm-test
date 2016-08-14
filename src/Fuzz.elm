module Fuzz exposing (Fuzzer, custom, constant, unit, bool, order, char, float, floatRange, int, tuple, tuple3, tuple4, tuple5, result, string, percentage, map, map2, andMap, andThen, maybe, intRange, list, array, frequency, frequencyOrCrash)

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
@docs bool, int, intRange, float, floatRange, percentage, string, maybe, result, list, array

## Working with Fuzzers
@docs Fuzzer, constant, map, map2, andMap, andThen, frequency, frequencyOrCrash

## Tuple Fuzzers
Instead of using a tuple, consider using `fuzzN`.
@docs tuple, tuple3, tuple4, tuple5

## Uncommon Fuzzers
@docs custom, char, unit, order

-}

import Array exposing (Array)
import Char
import Util exposing (..)
import Lazy.List exposing (LazyList)
import Shrink exposing (Shrinker)
import RoseTree exposing (RoseTree(..))
import Random.Pcg as Random exposing (Generator)
import Fuzz.Internal as Internal


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
        Internal.Fuzzer (Random.map shrinkTree generator)


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    custom (Random.constant ()) Shrink.noShrink


{-| A fuzzer for bool values.
-}
bool : Fuzzer Bool
bool =
    custom Random.bool Shrink.bool


{-| A fuzzer for order values.
-}
order : Fuzzer Order
order =
    let
        intToOrder i =
            if i == 0 then
                LT
            else if i == 1 then
                EQ
            else
                GT
    in
        custom (Random.map intToOrder (Random.int 0 2)) Shrink.order


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


{-| A fuzzer for int values within between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.
-}
intRange : Int -> Int -> Fuzzer Int
intRange min max =
    custom
        (Random.frequency
            [ ( 8, Random.int min max )
            , ( 1, Random.constant min )
            , ( 1, Random.constant max )
            ]
        )
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.int)


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.
-}
float : Fuzzer Float
float =
    let
        generator =
            Random.frequency
                [ ( 3, Random.float -50 50 )
                , ( 0.5, Random.constant 0 )
                , ( 1, Random.float -1 1 )
                , ( 1, Random.float 0 (toFloat <| Random.maxInt - Random.minInt) )
                , ( 1, Random.float (toFloat <| Random.minInt - Random.maxInt) 0 )
                ]
    in
        custom generator Shrink.float


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.
-}
floatRange : Float -> Float -> Fuzzer Float
floatRange min max =
    custom
        (Random.frequency
            [ ( 8, Random.float min max )
            , ( 1, Random.constant min )
            , ( 1, Random.constant max )
            ]
        )
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.float)


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`. It will test zero and one about 10% of the time each.
-}
percentage : Fuzzer Float
percentage =
    let
        generator =
            Random.frequency
                [ ( 8, Random.float 0 1 )
                , ( 1, Random.constant 0 )
                , ( 1, Random.constant 1 )
                ]
    in
        custom generator Shrink.float


{-| A fuzzer for char values. Generates random ascii chars disregarding the control
characters.
-}
char : Fuzzer Char
char =
    custom charGenerator Shrink.character


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
maybe (Internal.Fuzzer genTree) =
    Internal.Fuzzer <|
        Random.map2
            (\useNothing tree ->
                if useNothing then
                    RoseTree.singleton Nothing
                else
                    RoseTree.map Just tree
            )
            (Random.oneIn 4)
            genTree


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result (Internal.Fuzzer genError) (Internal.Fuzzer genValue) =
    Internal.Fuzzer <|
        Random.map3
            (\useError errorTree valueTree ->
                if useError then
                    RoseTree.map Err errorTree
                else
                    RoseTree.map Ok valueTree
            )
            (Random.oneIn 4)
            genError
            genValue


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Fuzzer a -> Fuzzer (List a)
list (Internal.Fuzzer genTree) =
    let
        genList ( weight, genInt ) =
            ( weight, genInt `Random.andThen` \i -> Random.list i (Random.map RoseTree.root genTree) )
    in
        custom
            (Random.frequency
                (List.map genList
                    [ ( 1, Random.constant 0 )
                    , ( 1, Random.constant 1 )
                    , ( 3, Random.int 2 10 )
                    , ( 2, Random.int 10 100 )
                    , ( 0.5, Random.int 100 400 )
                    ]
                )
            )
            -- TODO: shrink
            (Shrink.noShrink)


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array (Internal.Fuzzer genTree) =
    -- TODO: almost identical to list; DRY up?
    let
        genArray ( weight, genInt ) =
            ( weight, genInt `Random.andThen` \i -> (Random.map Array.fromList (Random.list i (Random.map RoseTree.root genTree))) )
    in
        custom
            (Random.frequency
                (List.map genArray
                    [ ( 1, Random.constant 0 )
                    , ( 1, Random.constant 1 )
                    , ( 3, Random.int 2 10 )
                    , ( 2, Random.int 10 100 )
                    , ( 0.5, Random.int 100 400 )
                    ]
                )
            )
            -- TODO: shrink
            (Shrink.noShrink)


{-| Turn a tuple of fuzzers into a fuzzer of tuples.
-}
tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( Internal.Fuzzer genA, Internal.Fuzzer genB ) =
    Internal.Fuzzer (Random.map2 (RoseTree.map2 (,)) genA genB)


{-| Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.
-}
tuple3 : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
tuple3 ( Internal.Fuzzer genA, Internal.Fuzzer genB, Internal.Fuzzer genC ) =
    Internal.Fuzzer (Random.map3 (RoseTree.map3 (,,)) genA genB genC)


{-| Turn a 4-tuple of fuzzers into a fuzzer of 4-tuples.
-}
tuple4 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d ) -> Fuzzer ( a, b, c, d )
tuple4 ( Internal.Fuzzer genA, Internal.Fuzzer genB, Internal.Fuzzer genC, Internal.Fuzzer genD ) =
    Internal.Fuzzer (Random.map4 (RoseTree.map4 (,,,)) genA genB genC genD)


{-| Turn a 5-tuple of fuzzers into a fuzzer of 5-tuples.
-}
tuple5 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d, Fuzzer e ) -> Fuzzer ( a, b, c, d, e )
tuple5 ( Internal.Fuzzer genA, Internal.Fuzzer genB, Internal.Fuzzer genC, Internal.Fuzzer genD, Internal.Fuzzer genE ) =
    Internal.Fuzzer (Random.map5 (RoseTree.map5 (,,,,)) genA genB genC genD genE)


{-| Create a fuzzer that only and always returns the value provided, and performs no shrinking. This is hardly random,
and so this function is best used as a helper when creating more complicated fuzzers.
-}
constant : a -> Fuzzer a
constant x =
    Internal.Fuzzer (Random.constant (RoseTree.singleton x))


{-| Map a function over a fuzzer. This applies to both the generated and the shruken values.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map f (Internal.Fuzzer genTree) =
    Internal.Fuzzer (Random.map (RoseTree.map f) genTree)


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 f (Internal.Fuzzer genTreeA) (Internal.Fuzzer genTreeB) =
    Internal.Fuzzer
        (Random.map2
            (\(Rose a moreAs) (Rose b moreBs) ->
                Rose (f a b) <| Lazy.List.map2 (RoseTree.map2 f) moreAs moreBs
            )
            genTreeA
            genTreeB
        )


{-| Map over many fuzzers.
-}
andMap : Fuzzer (a -> b) -> Fuzzer a -> Fuzzer b
andMap =
    map2 (<|)


{-| Create a fuzzer based on the result of another fuzzer.
-}
andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen f (Internal.Fuzzer genTree) =
    Internal.Fuzzer
        (Random.andThen
            genTree
            (\(Rose root branches) ->
                let
                    unpack : Fuzzer x -> Generator (RoseTree x)
                    unpack (Internal.Fuzzer genTree) =
                        genTree

                    -- genOtherChildren : Generator (LazyList (RoseTree b))
                    genOtherChildren =
                        branches
                            |> Lazy.List.map (\rt -> RoseTree.map (f >> unpack) rt |> unwindRoseTree)
                            |> unwindLazyList
                            |> Random.map (Lazy.List.map RoseTree.flatten)
                in
                    Random.map2
                        (\(Rose trueRoot root'sChildren) otherChildren ->
                            Rose trueRoot (Lazy.List.append root'sChildren otherChildren)
                        )
                        (unpack (f root))
                        genOtherChildren
            )
        )


unwindRoseTree : RoseTree (Generator a) -> Generator (RoseTree a)
unwindRoseTree (Rose genRoot lazyListOfRoseTreesOfGenerators) =
    case Lazy.List.headAndTail lazyListOfRoseTreesOfGenerators of
        Nothing ->
            Random.map RoseTree.singleton genRoot

        Just ( Rose gen children, moreList ) ->
            Random.map4 (\a b c d -> Rose a (Lazy.List.cons (Rose b c) d))
                genRoot
                gen
                (Lazy.List.map unwindRoseTree children |> unwindLazyList)
                (Lazy.List.map unwindRoseTree moreList |> unwindLazyList)


unwindLazyList : LazyList (Generator a) -> Generator (LazyList a)
unwindLazyList lazyListOfGenerators =
    case Lazy.List.headAndTail lazyListOfGenerators of
        Nothing ->
            Random.constant Lazy.List.empty

        Just ( head, tail ) ->
            Random.map2 (Lazy.List.cons) head (unwindLazyList tail)


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.

For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

This returns a `Result` because it can fail in a few ways:

* If you provide an empy list of frequencies
* If any of the weights are less than 0
* If the weights sum to 0

Any of these will lead to a result of `Err`, with a `String` explaining what
went wrong.
-}
frequency : List ( Float, Fuzzer a ) -> Result String (Fuzzer a)
frequency list =
    if List.isEmpty list then
        Err "You must provide at least one frequency pair."
    else if List.any (\( weight, _ ) -> weight < 0) list then
        Err "No frequency weights can be less than 0."
    else if List.sum (List.map fst list) <= 0 then
        Err "Frequency weights must sum to more than 0."
    else
        list
            |> List.map (\( weight, Internal.Fuzzer gen ) -> ( weight, gen ))
            |> Random.frequency
            |> Internal.Fuzzer
            |> Ok


{-| Calls `frequency` and handles `Err` results by crashing with the given
error message.

This is useful in tests, where a crash will simply cause the test run to fail.
There is no danger to a production system there.
-}
frequencyOrCrash : List ( Float, Fuzzer a ) -> Fuzzer a
frequencyOrCrash =
    frequency >> okOrCrash


okOrCrash : Result String a -> a
okOrCrash result =
    case result of
        Ok a ->
            a

        Err str ->
            Debug.crash str
