module Fuzz exposing (Fuzzer, custom, constant, unit, bool, order, char, float, floatRange, int, tuple, tuple3, tuple4, tuple5, result, string, percentage, map, map2, map3, map4, map5, andMap, andThen, maybe, intRange, list, array, frequency, frequencyOrCrash)

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
@docs Fuzzer, constant, map, map2, map3,map4, map5, andMap, andThen, frequency, frequencyOrCrash

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
            (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))

Here is an example for a custom union type, assuming there is already a `genName : Generator String` defined:

    type Question
        = Name String
        | Age Int

    question =
        let
            generator =
                Random.bool |> Random.andThen (\b ->
                    if b then
                        Random.map Name genName
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

It is not possible to extract the generator and shrinker from an existing fuzzer.
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


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or `-Infinity`.

It's possible for this fuzzer to generate any 32-bit integer, but it favors
numbers between -50 and 50 and especially zero.
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

Remember that [Random.maxInt](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#maxInt)
is the maximum possible int value, so you can do `intRange x Random.maxInt` to get all
the ints x or bigger.
-}
intRange : Int -> Int -> Fuzzer Int
intRange lo hi =
    custom
        (Random.frequency
            [ ( 8, Random.int lo hi )
            , ( 1, Random.constant lo )
            , ( 1, Random.constant hi )
            ]
        )
        (Shrink.keepIf (\i -> i >= lo && i <= hi) Shrink.int)


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.


It's possible for this fuzzer to generate any other floating-point value, but it
favors numbers between -50 and 50, numbers between -1 and 1, and especially zero.
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
floatRange lo hi =
    custom
        (Random.frequency
            [ ( 8, Random.float lo hi )
            , ( 1, Random.constant lo )
            , ( 1, Random.constant hi )
            ]
        )
        (Shrink.keepIf (\i -> i >= lo && i <= hi) Shrink.float)


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


{-| Generates random printable ASCII strings of up to 1000 characters.

Shorter strings are more common, especially the empty string.
-}
string : Fuzzer String
string =
    let
        generator : Generator String
        generator =
            Random.frequency
                [ ( 3, Random.int 1 10 )
                , ( 0.2, Random.constant 0 )
                , ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
                |> Random.andThen (lengthString charGenerator)
    in
        custom generator Shrink.string


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe (Internal.Fuzzer baseFuzzer) =
    Internal.Fuzzer <|
        \noShrink ->
            case baseFuzzer noShrink of
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
                                    RoseTree.map Just tree |> RoseTree.addChild (RoseTree.singleton Nothing)
                            )
                            (Random.oneIn 4)
                            genTree


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result (Internal.Fuzzer baseFuzzerError) (Internal.Fuzzer baseFuzzerValue) =
    Internal.Fuzzer <|
        \noShrink ->
            case ( baseFuzzerError noShrink, baseFuzzerValue noShrink ) of
                ( Gen genErr, Gen genVal ) ->
                    Gen <|
                        Random.map3
                            (\useError err val ->
                                if useError then
                                    Err err
                                else
                                    Ok val
                            )
                            (Random.oneIn 4)
                            genErr
                            genVal

                ( Shrink genTreeErr, Shrink genTreeVal ) ->
                    Shrink <|
                        Random.map3
                            (\useError errorTree valueTree ->
                                if useError then
                                    RoseTree.map Err errorTree
                                else
                                    RoseTree.map Ok valueTree
                            )
                            (Random.oneIn 4)
                            genTreeErr
                            genTreeVal

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.result" err


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Fuzzer a -> Fuzzer (List a)
list (Internal.Fuzzer baseFuzzer) =
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
                case baseFuzzer noShrink of
                    Gen genVal ->
                        genLength
                            |> Random.andThen (\i -> (Random.list i genVal))
                            |> Gen

                    Shrink genTree ->
                        genLength
                            |> Random.andThen (\i -> (Random.list i genTree))
                            |> Random.map listShrinkHelp
                            |> Shrink
            )


listShrinkHelp : List (RoseTree a) -> RoseTree (List a)
listShrinkHelp listOfTrees =
    {- Shrinking a list of RoseTrees
       We need to do two things. First, shrink individual values. Second, shorten the list.
       To shrink individual values, we create every list copy of the input list where any
       one value is replaced by a shrunken form.
       To shorten the length of the list, slide windows of various lengths over it.
       In all cases, recurse! The goal is to make a little forward progress and then recurse.
    -}
    let
        n =
            List.length listOfTrees

        root =
            List.map RoseTree.root listOfTrees

        shrinkOne prefix list =
            case list of
                [] ->
                    Lazy.List.empty

                (Rose x shrunkenXs) :: more ->
                    Lazy.List.map (\childTree -> prefix ++ (childTree :: more) |> listShrinkHelp) shrunkenXs

        shrunkenVals =
            Lazy.List.numbers
                |> Lazy.List.map (\i -> i - 1)
                |> Lazy.List.take n
                |> Lazy.List.andThen
                    (\i -> shrinkOne (List.take i listOfTrees) (List.drop i listOfTrees))

        shortened =
            (if n > 6 then
                Lazy.List.iterate (\n -> n // 2) n
                    |> Lazy.List.takeWhile (\x -> x > 0)
             else
                Lazy.List.fromList (List.range 1 n)
            )
                |> Lazy.List.andThen (\len -> shorter len listOfTrees False)
                |> Lazy.List.map listShrinkHelp

        shorter windowSize aList recursing =
            -- Tricky: take the whole list if we've recursed down here, but don't let a list shrink to itself
            if windowSize > List.length aList || (windowSize == List.length aList && not recursing) then
                Lazy.List.empty
            else
                case aList of
                    [] ->
                        Lazy.List.empty

                    head :: tail ->
                        Lazy.List.cons (List.take windowSize aList) (shorter windowSize tail True)
    in
        Lazy.List.append shortened shrunkenVals
            |> Lazy.List.cons (RoseTree.singleton [])
            |> Rose root


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array fuzzer =
    map Array.fromList (list fuzzer)


{-| Turn a tuple of fuzzers into a fuzzer of tuples.
-}
tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( Internal.Fuzzer baseFuzzerA, Internal.Fuzzer baseFuzzerB ) =
    Internal.Fuzzer
        (\noShrink ->
            case ( baseFuzzerA noShrink, baseFuzzerB noShrink ) of
                ( Gen genA, Gen genB ) ->
                    Gen <| Random.map2 (,) genA genB

                ( Shrink genTreeA, Shrink genTreeB ) ->
                    Shrink <| Random.map2 tupleShrinkHelp genTreeA genTreeB

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.tuple" err
        )


tupleShrinkHelp : RoseTree a -> RoseTree b -> RoseTree ( a, b )
tupleShrinkHelp ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) =
    {- Shrinking a tuple of RoseTrees
       Recurse on all tuples created by substituting one element for any of its shrunken values.

       A weakness of this algorithm is that it expects that values can be shrunken independently.
       That is, to shrink from (a,b) to (a',b'), we must go through (a',b) or (a,b').
       "No pairs sum to zero" is a pathological predicate that cannot be shrunken this way.
    -}
    let
        root =
            ( root1, root2 )

        shrink1 =
            Lazy.List.map (\subtree -> tupleShrinkHelp subtree rose2) children1

        shrink2 =
            Lazy.List.map (\subtree -> tupleShrinkHelp rose1 subtree) children2
    in
        shrink2
            |> Lazy.List.append shrink1
            |> Rose root


{-| Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.
-}
tuple3 : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
tuple3 ( Internal.Fuzzer baseFuzzerA, Internal.Fuzzer baseFuzzerB, Internal.Fuzzer baseFuzzerC ) =
    Internal.Fuzzer
        (\noShrink ->
            case ( baseFuzzerA noShrink, baseFuzzerB noShrink, baseFuzzerC noShrink ) of
                ( Gen genA, Gen genB, Gen genC ) ->
                    Gen <| Random.map3 (,,) genA genB genC

                ( Shrink genTreeA, Shrink genTreeB, Shrink genTreeC ) ->
                    Shrink <| Random.map3 tupleShrinkHelp3 genTreeA genTreeB genTreeC

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.tuple3" err
        )


tupleShrinkHelp3 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree ( a, b, c )
tupleShrinkHelp3 ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) ((Rose root3 children3) as rose3) =
    let
        root =
            ( root1, root2, root3 )

        shrink1 =
            Lazy.List.map (\subtree -> tupleShrinkHelp3 subtree rose2 rose3) children1

        shrink2 =
            Lazy.List.map (\subtree -> tupleShrinkHelp3 rose1 subtree rose3) children2

        shrink3 =
            Lazy.List.map (\subtree -> tupleShrinkHelp3 rose1 rose2 subtree) children3
    in
        shrink3
            |> Lazy.List.append shrink2
            |> Lazy.List.append shrink1
            |> Rose root


{-| Turn a 4-tuple of fuzzers into a fuzzer of 4-tuples.
-}
tuple4 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d ) -> Fuzzer ( a, b, c, d )
tuple4 ( Internal.Fuzzer baseFuzzerA, Internal.Fuzzer baseFuzzerB, Internal.Fuzzer baseFuzzerC, Internal.Fuzzer baseFuzzerD ) =
    Internal.Fuzzer
        (\noShrink ->
            case ( baseFuzzerA noShrink, baseFuzzerB noShrink, baseFuzzerC noShrink, baseFuzzerD noShrink ) of
                ( Gen genA, Gen genB, Gen genC, Gen genD ) ->
                    Gen <| Random.map4 (,,,) genA genB genC genD

                ( Shrink genTreeA, Shrink genTreeB, Shrink genTreeC, Shrink genTreeD ) ->
                    Shrink <| Random.map4 tupleShrinkHelp4 genTreeA genTreeB genTreeC genTreeD

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.tuple4" err
        )


tupleShrinkHelp4 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree ( a, b, c, d )
tupleShrinkHelp4 rose1 rose2 rose3 rose4 =
    let
        root =
            ( RoseTree.root rose1, RoseTree.root rose2, RoseTree.root rose3, RoseTree.root rose4 )

        shrink1 =
            Lazy.List.map (\subtree -> tupleShrinkHelp4 subtree rose2 rose3 rose4) (RoseTree.children rose1)

        shrink2 =
            Lazy.List.map (\subtree -> tupleShrinkHelp4 rose1 subtree rose3 rose4) (RoseTree.children rose2)

        shrink3 =
            Lazy.List.map (\subtree -> tupleShrinkHelp4 rose1 rose2 subtree rose4) (RoseTree.children rose3)

        shrink4 =
            Lazy.List.map (\subtree -> tupleShrinkHelp4 rose1 rose2 rose3 subtree) (RoseTree.children rose4)
    in
        shrink4
            |> Lazy.List.append shrink3
            |> Lazy.List.append shrink2
            |> Lazy.List.append shrink1
            |> Rose root


{-| Turn a 5-tuple of fuzzers into a fuzzer of 5-tuples.
-}
tuple5 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d, Fuzzer e ) -> Fuzzer ( a, b, c, d, e )
tuple5 ( Internal.Fuzzer baseFuzzerA, Internal.Fuzzer baseFuzzerB, Internal.Fuzzer baseFuzzerC, Internal.Fuzzer baseFuzzerD, Internal.Fuzzer baseFuzzerE ) =
    Internal.Fuzzer
        (\noShrink ->
            case ( baseFuzzerA noShrink, baseFuzzerB noShrink, baseFuzzerC noShrink, baseFuzzerD noShrink, baseFuzzerE noShrink ) of
                ( Gen genA, Gen genB, Gen genC, Gen genD, Gen genE ) ->
                    Gen <| Random.map5 (,,,,) genA genB genC genD genE

                ( Shrink genTreeA, Shrink genTreeB, Shrink genTreeC, Shrink genTreeD, Shrink genTreeE ) ->
                    Shrink <| Random.map5 tupleShrinkHelp5 genTreeA genTreeB genTreeC genTreeD genTreeE

                err ->
                    Debug.crash "This shouldn't happen: Fuzz.tuple5" err
        )


tupleShrinkHelp5 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e -> RoseTree ( a, b, c, d, e )
tupleShrinkHelp5 rose1 rose2 rose3 rose4 rose5 =
    let
        root =
            ( RoseTree.root rose1, RoseTree.root rose2, RoseTree.root rose3, RoseTree.root rose4, RoseTree.root rose5 )

        shrink1 =
            Lazy.List.map (\subtree -> tupleShrinkHelp5 subtree rose2 rose3 rose4 rose5) (RoseTree.children rose1)

        shrink2 =
            Lazy.List.map (\subtree -> tupleShrinkHelp5 rose1 subtree rose3 rose4 rose5) (RoseTree.children rose2)

        shrink3 =
            Lazy.List.map (\subtree -> tupleShrinkHelp5 rose1 rose2 subtree rose4 rose5) (RoseTree.children rose3)

        shrink4 =
            Lazy.List.map (\subtree -> tupleShrinkHelp5 rose1 rose2 rose3 subtree rose5) (RoseTree.children rose4)

        shrink5 =
            Lazy.List.map (\subtree -> tupleShrinkHelp5 rose1 rose2 rose3 rose4 subtree) (RoseTree.children rose5)
    in
        shrink5
            |> Lazy.List.append shrink4
            |> Lazy.List.append shrink3
            |> Lazy.List.append shrink2
            |> Lazy.List.append shrink1
            |> Rose root


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
map transform (Internal.Fuzzer baseFuzzer) =
    Internal.Fuzzer
        (\noShrink ->
            case baseFuzzer noShrink of
                Gen genVal ->
                    Gen <| Random.map transform genVal

                Shrink genTree ->
                    Shrink <| Random.map (RoseTree.map transform) genTree
        )


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 transform fuzzA fuzzB =
    map (\( a, b ) -> transform a b) (tuple ( fuzzA, fuzzB ))


{-| Map over three fuzzers.
-}
map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 transform fuzzA fuzzB fuzzC =
    map (\( a, b, c ) -> transform a b c) (tuple3 ( fuzzA, fuzzB, fuzzC ))


{-| Map over four fuzzers.
-}
map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 transform fuzzA fuzzB fuzzC fuzzD =
    map (\( a, b, c, d ) -> transform a b c d) (tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD ))


{-| Map over five fuzzers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 transform fuzzA fuzzB fuzzC fuzzD fuzzE =
    map (\( a, b, c, d, e ) -> transform a b c d e) (tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE ))


{-| Map over many fuzzers. This can act as mapN for N > 5.

The argument order is meant to accomodate chaining:

    map f aFuzzer
        |> andMap anotherFuzzer
        |> andMap aThirdFuzzer

Note that shrinking may be better using mapN.
-}
andMap : Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
andMap =
    map2 (|>)


{-| Create a fuzzer based on the result of another fuzzer.
-}
andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen transform (Internal.Fuzzer baseFuzzer) =
    Internal.Fuzzer
        (\noShrink ->
            case baseFuzzer noShrink of
                Gen genVal ->
                    Gen <| Random.andThen (transform >> Internal.unpackGenVal) genVal

                Shrink genTree ->
                    Shrink <| andThenRoseTrees transform genTree
        )


andThenRoseTrees : (a -> Fuzzer b) -> Generator (RoseTree a) -> Generator (RoseTree b)
andThenRoseTrees transform genTree =
    genTree
        |> Random.andThen
            (\(Rose root branches) ->
                let
                    genOtherChildren : Generator (LazyList (RoseTree b))
                    genOtherChildren =
                        branches
                            |> Lazy.List.map (\rt -> RoseTree.map (transform >> Internal.unpackGenTree) rt |> unwindRoseTree)
                            |> unwindLazyList
                            |> Random.map (Lazy.List.map RoseTree.flatten)
                in
                    Random.map2
                        (\(Rose trueRoot rootsChildren) otherChildren ->
                            Rose trueRoot (Lazy.List.append rootsChildren otherChildren)
                        )
                        (Internal.unpackGenTree (transform root))
                        genOtherChildren
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
            Random.map2 Lazy.List.cons head (unwindLazyList tail)


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
    else if List.sum (List.map Tuple.first list) <= 0 then
        Err "Frequency weights must sum to more than 0."
    else
        Ok <|
            Internal.Fuzzer <|
                \noShrink ->
                    if noShrink then
                        list
                            |> List.map (\( weight, fuzzer ) -> ( weight, Internal.unpackGenVal fuzzer ))
                            |> Random.frequency
                            |> Gen
                    else
                        list
                            |> List.map (\( weight, fuzzer ) -> ( weight, Internal.unpackGenTree fuzzer ))
                            |> Random.frequency
                            |> Shrink


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
